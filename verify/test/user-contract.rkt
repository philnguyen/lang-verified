#lang verify (5 2 4 19)

(define/contract (inc x)
  (integer? . -> . integer?)
  "42")
