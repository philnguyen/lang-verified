#lang verified

(define/contract (inc n)
  (integer? . -> . integer?)
  (add1 n))
