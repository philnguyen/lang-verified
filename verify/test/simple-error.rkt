#lang verify (5 2 "possible division by 0")

(define/contract (recip n)
  (number? . -> . number?)
  (/ 1 n))
