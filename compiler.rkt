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
  (compile-expr expr '())
  (emit-string (ret)))

(define (compile-expr expr env [emit emit-string])
  (match expr

    [(node/immediate 'integer num)
     (emit (movq (immediate num) (reg 'ret-val)))]

    [(node/prim type name arity args)
     (foreach-shortest (λ (arg reg-idx)
                 (compile-expr arg env emit) (emit (movq (reg 'ret-val) (reg reg-idx))))
               args '(param-1 param-2 param-3 param-4))
     (compile-prim type name arity env emit)]

    ))

(define (compile-prim type name arity env emit)
  (match name
    ['+
     (emit (addq (reg 'param-1) (reg 'param-2)))
     (emit (movq (reg 'param-2) (reg 'ret-val)))
     ]))

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

(define (foreach-shortest proc . lists)
  (if (findf (λ (l) (eq? l '())) lists)
      (void)
      (begin
        (apply proc (map car lists))
        (apply foreach-shortest (cons proc (map cdr lists))))))

[module+ test
  (let ([i 1])
    (foreach-shortest (λ (a b) (set! i (+ i a b))) '(2 4 8 16) '(32 64))
    (check-= i 103 0))]

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
