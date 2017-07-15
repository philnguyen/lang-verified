#lang racket/base

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [checked-module-begin #%module-begin]))

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/contract))

(begin-for-syntax

  (define error-name (make-parameter 'contract-violation))

  ;; Search for sub-expression that starts at line `l` columne `c` in `form`
  (define/contract (search-subform l c form)
    (integer? integer? syntax? . -> . (or/c syntax? #f))

    (define (search-by-col form)
      (if (= (syntax-column form) c)
          form
          (syntax-parse form
            [(subforms ...) (ormap search-by-col (syntax->list #'(subforms ...)))]
            [_ #f])))

    (let search-by-line ([form form])
      (cond [(= (syntax-line form) l) (search-by-col form)]
            [(< (syntax-line form) l)
             (syntax-parse form
               [(subforms ...) (ormap search-by-line (syntax->list #'(subforms ...)))]
               [_ #f])]
            [else #f])))

  ;; Search for sub-expresson that starts at line `l` column `c` in `form` and blame
  (define/contract (maybe-raise-error l c msg form)
    (integer? integer? string? syntax? . -> . syntax?)
    (define ?subform (search-subform l c form))
    (if ?subform
        (raise-syntax-error (error-name) msg ?subform)
        (error 'maybe-raise-error "no syntax that starts at line ~a column ~a" l c))))

;; Check syntax before passing to racket's #%module-begin
(define-syntax checked-module-begin
  (syntax-parser
    [(_ (line:integer col:integer msg:str) forms ...)
     (for ([form (syntax->list #'(forms ...))])
       (maybe-raise-error (syntax-e #'line) (syntax-e #'col) (syntax-e #'msg) form))
     #'(#%module-begin forms ...)]))
