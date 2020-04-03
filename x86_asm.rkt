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

(define bool-shift 7)
(define bool-tag 31)

(define true-rep 159)
(define false-rep 31)

(define wordsize 8)                     ;Assuming 64-bit architecture

;; These by and large just format the assembly

(define (movq val dest)
  ;; TODO: I'd like to generalize this so that it can handle memory locations
  (format "movq ~a, ~a" val dest))

(define (addq reg-a reg-b)
  (format "addq ~a, ~a" reg-a reg-b))

(define (subq reg-a reg-b)
  (format "subq ~a, ~a" reg-a reg-b))

(define (mulq reg-a reg-b)
  ;; This emits two instructions: first the multiply instruction, then
  ;; a shift instruction to correct
  (format "imulq ~a, ~a\nshr $~a, ~a" reg-a reg-b fixnum-shift reg-b))

(define (num-equals reg-a reg-b)
  (format "cmpq ~a, ~a\nsete ~a\nmovzbq ~a, ~a\nshl $~a, ~a\nxorq $~a, ~a"
          reg-a reg-b                         ; cmpq args
          (reg 'ret-val-small)                ; sete
          (reg 'ret-val-small) (reg 'ret-val) ; movzbq
          bool-shift (reg 'ret-val)           ; shl
          bool-tag (reg 'ret-val)             ; xorq
          ))

(define (prim-bin-op name)
  (match name
    ['+ addq]
    ['- subq]
    ['* mulq]
    ['= num-equals]))

(define (cmpq a b)
  (format "cmpq ~a, ~a" a b))

(define (je loc)
  (format "je _~a" loc))

(define (jmp loc)
  (format "jmp _~a" loc))

(define (orq reg1 reg2)
  (format "orq ~a, ~a" reg1 reg2))

;; convert val to its immediate representation
(define (immediate val)
  (match val
    [(? integer?)
     (format "$~a" (arithmetic-shift val fixnum-shift))]
    [(? boolean?)
     (format "$~a" (if val true-rep false-rep))]))

;; Like `immeidate`, but doesn't do any converting
(define (raw-immediate val)
  (format "$~a" val))

;; Tag the current heap pointer with its type
(define (tag-heap kind)
  (match kind
    ['pair (orq (raw-immediate 1) (reg 'heap))]))

(define (label sym)
  (format "_~a:" sym))

(define (function-label name)
  (gensym (string-append "func_" (symbol->string name))))

(define (ret) "ret")

(define (call label)
  (format "call _~a" label))

(define (push place)
  (format "push ~a" place))

(define (pop place)
  (format "pop ~a" place))


(define (fresh-label)
  (gensym (string-append "l_")))

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

(define (heap [offset (- wordsize)])
  (mem #:offset offset #:reg-b (reg 'heap)))

(define (reg reg-name)
  (match reg-name
    ['ret-val        "%rax"]
    ['ret-val-small  "%al"]
    ['stack          "%rsp"]
    ['heap           "%r15"]
    ['param-1        "%rdi"]
    ['param-2        "%rsi"]
    ['param-3        "%rdx"]
    ['param-4        "%rcx"]
    ['param-5        "%r9"]
    ['param-6        "%r10"]
    ['param-7        "%r11"]
    ['param-8        "%r12"]))

[module+ test
  (check-equal? (mem #:offset 42) "42")
  (check-equal? (mem #:offset 42 #:reg-i (reg 'param-1) #:s 4) "42(,%rdi,4)")
  (check-equal? (mem #:reg-b (reg 'param-1)) "(%rdi)")
  ]
