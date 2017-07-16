#lang verify

(define/contract (inc n)
  (integer? . -> . integer?)
  (add1 n))
