#lang racket/base

(provide new-compiler-env fresh-label fresh-tmp)

;; The environment holds information about variables and forms. (e.g.
;; keeps references to function labels so we can compile those into
;; function calls)
(define (new-compiler-env)
  '())

(define (fresh-label) (gensym ℓ))
(define (fresh-tmp) (gensym 'tmp))
