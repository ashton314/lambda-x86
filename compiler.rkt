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
  (compile-ast (parse expr) 0 '())
  (emit (ret)))

(define (compile-ast ast stack env)
  (match ast
    [(node/immediate _ num)
     (emit (movq (immediate num) (reg 'ret-val)))]

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[module+ test
  (check-equal? (crc 42) "42")
  (check-equal? (crc 45) "45")]

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
  (emitter ".text
	.p2align 4,,15
	.globl _scheme_entry
_scheme_entry:"))
