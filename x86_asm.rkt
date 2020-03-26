#lang racket/base
(require racket/match)
(provide (all-defined-out))

(module+ test
  (require rackunit))

;; constants
(define fixnum-shift 2)
(define fixnum-tag 0)
(define fixnum-mask 3)
(define empty-list 47)

(define wordsize 8)                     ;Assuming 64-bit architecture

;; These by and large just format the assembly

(define (movq val dest)
  ;; TODO: I'd like to generalize this so that it can handle memory locations
  (format "movq ~a, ~a" val dest))

(define (addq reg-a reg-b)
  (format "addq ~a, ~a" reg-a reg-b))

(define (immediate int)
  (format "$~a" (arithmetic-shift int fixnum-shift)))

(define (ret) "ret")

(define (mem #:offset [imm #f] #:reg-b [rb #f] #:reg-i [ri #f] #:s [s #f])
  (string-append
   (if imm (format "~a" imm) "")
   (if (or rb ri s)
       (format "(~a)"
               (string-append
                (if rb (format "~a" rb) "")
                (if ri (format ",~a" ri) "")
                (if s (format ",~a" s) "")))
       "")))

(define (stack [offset (- wordsize)])
  (mem #:offset offset #:reg-b (reg 'stack)))

(define (reg reg-name)
  (match reg-name
    ['ret-val "%rax"]
    ['stack   "%rsp"]
    ['param-1 "%rdi"]
    ['param-2 "%rsi"]
    ['param-3 "%rdx"]
    ['param-4 "%rcx"]))

[module+ test
  (check-equal? (mem #:offset 42) "42")
  (check-equal? (mem #:offset 42 #:reg-i (reg 'param-1) #:s 4) "42(,%rdi,4)")
  (check-equal? (mem #:reg-b (reg 'param-1)) "(%rdi)")
  ]
