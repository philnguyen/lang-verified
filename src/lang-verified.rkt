#lang racket/base

(require racket/contract
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/contract))

(provide (except-out (all-from-out racket/base) #%module-begin)
         (all-from-out racket/contract)
         (rename-out [checked-module-begin #%module-begin]))

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

  (define (search-subform/error l c form tag)
    (cond [(search-subform l c form) => values]
          [else (error tag "no syntax that starts at line ~a column ~a" l c)]))

  ;; Search for sub-expresson that starts at line `l` column `c` in `form` and blame
  (define/contract (raise-simple-error l c msg form)
    (integer? integer? string? syntax? . -> . void?)
    (define subform (search-subform/error l c form 'raise-simple-error))
    (raise-syntax-error (error-name) msg subform))

  (define/contract (raise-contract-error l c ctc-l ctc-c form)
    (integer? integer? integer? integer? syntax? . -> . void?)
    (define error-site (search-subform/error     l     c form 'raise-contract-error))
    (define ctc-site   (search-subform/error ctc-l ctc-c form 'raise-contract-error))
    (raise-syntax-error (error-name)
                        (format "contract violation with `~a`" (syntax->datum ctc-site))
                        error-site
                        #f
                        (list ctc-site))))

;; Check syntax before passing to racket's #%module-begin
(define-syntax checked-module-begin
  (syntax-parser
    [(_ (line:integer col:integer msg:str) forms ...)
     (for ([form (syntax->list #'(forms ...))])
       (raise-simple-error (syntax-e #'line) (syntax-e #'col) (syntax-e #'msg) form))
     #'(#%module-begin forms ...)]
    [(_ (line:integer col:integer ctc-line:integer ctc-col:integer) forms ...)
     (for ([form (syntax->list #'(forms ...))])
       (raise-contract-error (syntax-e #'line) (syntax-e #'col)
                                   (syntax-e #'ctc-line) (syntax-e #'ctc-col)
                                   form))
     #'(#%module-begin forms ...)]
    [(_ forms ...) ; no errror case
     #'(#%module-begin forms ...)]))
