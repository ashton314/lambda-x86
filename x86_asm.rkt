#lang racket/base
(require racket/match)
(provide (all-defined-out))

(module+ test
  (require rackunit))

;; constants
(define fixnum-shift 2)
(define fixnum-tag 0)
(define fixnum-mask 3)
(define empty-list )

;; These by and large just format the assembly

(define (movq val dest)
  ;; TODO: I'd like to generalize this so that it can handle memory locations
  (format "movq ~a, ~a" val dest))

(define (addq reg-a reg-b)
  (format "addq ~a, ~a" reg-a reg-b))

(define (immediate int)
  (format "$~a" (arithmetic-shift int fixnum-shift)))

(define (ret) "ret")

(define (reg reg-name)
  (match reg-name
    ['ret-val "%rax"]
    ['param-1 "%rdi"]
    ['param-2 "%rsi"]
    ['param-3 "%rdx"]
    ['param-4 "%rcx"]))
