#lang racket
(require "ast.rkt")
(require "type_checker.rkt")
(require "x86_asm.rkt")

(module+ test
  (require rackunit))

;; Constants
(define asm-file "compiler-output.s")

(define-syntax-rule (c expr)
  ;; Macro to make this easier
  (compile (quote expr)))

(define (compile expr)
  ;; Parse, and then compile the resulting expression
  (delete-file asm-file)
  (global-prelude)
  (compile-ast (parse expr) (- wordsize) '())
  (emit (ret)))

(define (compile-ast ast stack env)
  ;; Primary copmiler dispatch: each AST node is matched here and
  ;; either compiled inline or compiled separately by the respective
  ;; routines
  (match ast
    [(node/immediate _ num)
     (emit (movq (immediate num) (reg 'ret-val)))]

    [(node/prim _ name arity args)
     (compile-primitive stack env name arity args)]

    ))

(define (compile-primitive stack-bottom env name arity args)
  (match name
    ['add1
     (compile-ast (car args) stack-bottom env)
     (emit (addq (immediate 1) (reg 'ret-val)))]

    ['zero?
     (compile-ast (car args) stack-bottom env)
     (emit (num-equals (immediate 0) (reg 'ret-val)))
     ]

    [(? (λ (o) (member o '(+ - * /))) op)
     ;; Funny order because of `-`
     (compile-ast (cadr args) stack-bottom env)
     (emit (movq (reg 'ret-val) (stack stack-bottom)))
     (compile-ast (car args) (- stack-bottom wordsize) env)
     (emit ((prim-bin-op op) (stack stack-bottom) (reg 'ret-val)))
     ]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[module+ test
  ;; Compile constants
  (check-equal? (crc 42) "42")
  (check-equal? (crc 45) "45")

  ;; Primitive unary operators
  (check-equal? (crc '(add1 5)) "6")
  (check-equal? (crc '(zero? 5)) "#f")
  (check-equal? (crc '(zero? 0)) "#t")

  ;; Primitive binary operators
  (check-equal? (crc '(+ 1 2)) "3")
  (check-equal? (crc '(+ (+ 1 2) (+ 3 4))) "10")
  (check-equal? (crc '(- 2 1)) "1")
  (check-equal? (crc '(- (* 2 3) 1)) "5")
  ]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (crc expr)
  ;; CRC: *C*ompile, *R*un, *C*apture output
  (compile expr)
  (system "gcc compiler-output.s driver.c")
  (with-output-to-string (lambda () (system "./a.out"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (write-to-asm thing)
  #;(println (list 'emit: thing))
  (with-output-to-file asm-file (λ () (if (not (string-suffix? thing ":")) (display "\t") (void)) (displayln thing)) #:exists 'append))

(define (emit-string thing [writer write-to-asm])
  (writer thing))

(define (emit thing) (emit-string thing))

(define (global-prelude [emitter emit-string])
  (emitter "	.text
	.p2align 4,,15
	.globl _scheme_entry
_scheme_entry:"))
