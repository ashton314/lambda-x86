#lang racket/base
(require racket/match racket/pretty)
(require "ast.rkt")
(require "type_checker.rkt")
(require "x86_asm.rkt")

(module+ test
  (require rackunit))

(provide compile)

(define asm-file "compiler-output.s")

(define (compile expr)
  (delete-file asm-file)
  (global-prelude)

  (compile-expr expr '()))

(define (compile-expr expr env [emit emit-string])
  (match expr

    [(node/immediate 'integer num)
     (begin
       (emit (movq (immediate num) (reg 'ret-val)))
       (emit (ret)))]

    ))

(define (env/new) '())

;; Returns the type of a variable from the env
(define (env/var-type env var)
  (match env
    [(list) #f]                   ; Not in env
    [(cons (cons v t) rst)
     #:when (eq? v var)
     t]
    [(cons _ rst) (env/var-type rst var)]))

;; Extend a env with a value
(define (env/extend env var type)
  (cons (cons var type) env))

[module+ test
  (let ([ctx (env/extend (env/new) 'foo 'integer)])
    (check-eq? (env/var-type ctx 'foo) 'integer)
    (check-eq? (env/var-type (env/extend ctx 'bar 'string) 'foo) 'integer)
    (check-eq? (env/var-type (env/extend ctx 'bar 'string) 'bar) 'string))]

(define (write-to-asm thing)
  (println (list 'thing thing))
  (with-output-to-file asm-file (λ () (displayln thing)) #:exists 'append))

(define (emit-string thing [writer write-to-asm])
  (writer thing))

(define (global-prelude [emitter emit-string])
  (emitter "	.text
        .p2align 4,,15
	.globl _scheme_entry
_scheme_entry:
"))

[module+ test
  (check-eqv? (global-prelude (λ (x) x)) "	.text
        .p2align 4,,15
	.globl _scheme_entry
_scheme_entry:
")]
