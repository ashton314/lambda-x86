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

    ;; Constants
    [(node/immediate _ num)
     (emit (movq (immediate num) (reg 'ret-val)))]

    ;; Primitives
    [(node/prim _ name arity args)
     (compile-primitive stack env name arity args)]

    ;; Let bindings
    [(node/let _ bindings body)
     (compile-let* stack env bindings body)]

    ;; Variable references
    [(node/var _ name)
     (emit (movq (or (env/lookup env name) (error 'undefined-variable)) (reg 'ret-val)))]

    ;; Conditionals
    [(node/if _ condition t-case f-case)
     (compile-if stack env condition t-case f-case)]
    ))

(define (compile-primitive stack-bottom env name arity args)
  ;; Compile our primitive operations: unary operators like `add1' and
  ;; `zero?', as well as our binary operators
  (match name
    ['add1
     (compile-ast (car args) stack-bottom env)
     (emit (addq (immediate 1) (reg 'ret-val)))]

    ['zero?
     (compile-ast (car args) stack-bottom env)
     (emit (num-equals (immediate 0) (reg 'ret-val)))]

    ['cons
     (compile-ast (car args) stack-bottom env)
     (emit (movq (reg 'ret-val) (heap 0)))
     (compile-ast (cadr args) stack-bottom env)
     (emit (movq (reg 'ret-val) (heap wordsize)))
     (emit (movq (reg 'heap) (reg 'ret-val)))
     (emit (orq (raw-immediate 1) (reg 'ret-val)))                 ; tag our return value as pointing to a pair
     (emit (addq (raw-immediate (* 2 wordsize)) (reg 'heap)))]

    [(? (λ (o) (member o '(+ - * /))) op)
     ;; Funny order because of `-`
     (compile-ast (cadr args) stack-bottom env)
     (emit (movq (reg 'ret-val) (stack stack-bottom)))
     (compile-ast (car args) (- stack-bottom wordsize) env)
     (emit ((prim-bin-op op) (stack stack-bottom) (reg 'ret-val)))]
    ))

(define (compile-let* stack-bottom env bindings body)
  (if (null? bindings)
      (compile-ast body stack-bottom env)
      ;; Emit code for one binding
      ;; Remember: we asssume stack-bottom is always free
      (let ([binding (car bindings)])
        (compile-ast (node/let-binding-value binding) stack-bottom env)
        (emit (movq (reg 'ret-val) (stack stack-bottom)))
        (compile-let* (- stack-bottom wordsize)
                      (env/extend env (node/let-binding-variable binding) (stack stack-bottom))
                      (cdr bindings)
                      body))))

(define (compile-if stack-bottom env condition t-case f-case)
  (let ([l-false (gensym 'false_branch)]
        [l-end   (gensym 'if_end)])
    (compile-ast condition stack-bottom env)
    (emit (cmpq (immediate #f) (reg 'ret-val)))
    (emit (je l-false))
    (compile-ast t-case stack-bottom env)
    (emit (jmp l-end))
    (emit (label l-false))
    (compile-ast f-case stack-bottom env)
    (emit (label l-end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (env/new) '())

(define (env/extend env var location)
  (cons (cons var location) env))

(define (env/lookup env var)
  (match env
    ['() #f]
    [(cons (cons v l) rest-env)
     (if (eq? v var) l (env/lookup rest-env var))]))

[module+ test
  (let ([ctx (env/extend (env/new) 'foo 'integer)])
    (check-eq? (env/lookup ctx 'foo) 'integer)
    (check-eq? (env/lookup (env/extend ctx 'bar 'string) 'foo) 'integer)
    (check-eq? (env/lookup (env/extend ctx 'bar 'string) 'bar) 'string))]

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

  ;; Let bindings
  (check-equal? (crc '(let ((x 1)) x)) "1")
  (check-equal? (crc '(let ((x (+ 1 2))) x)) "3")
  (check-equal? (crc '(let ((x (+ 1 2))) (* x 2))) "6")
  (check-equal? (crc '(let ((x 1) (y 2)) (+ x y))) "3")
  (check-equal? (crc '(let ((x 2) (y 3)) (- (* x y) (+ x y)))) "1")
  (check-equal? (crc '(let ((x 2)) (+ x (let ((y 3) (z 4)) (+ (* x y) z))))) "12")
  ;; check shadowing
  (check-equal? (crc '(let ((x 2) (y 3))
                        (+ (+ x y)
                           (let ((y 10))
                             (* x y))))) "25")

  ;; Conditionals
  (check-equal? (crc '(if #t 1 2)) "1")
  (check-equal? (crc '(if #f 1 2)) "2")
  (check-equal? (crc '(if (zero? (- 2 1)) 1 2)) "2")
  (check-equal? (crc '(if (zero? (- 2 2)) 1 2)) "1")
  (check-equal? (crc '(let ((x 2) (y 3))
                        (let ((z (if (zero? (- (* x y) 6)) 1 2)))
                          (* z 100)))) "100")

  ;; Cons
  (check-equal? (crc '(cons 1 2)) "(1 . 2)")
  (check-equal? (crc '(cons 1 (+ 1 2))) "(1 . 3)")
  (check-equal? (crc '(cons (* 2 3) (+ 1 2))) "(6 . 3)")
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
  (with-output-to-file asm-file (λ () (if (not (string-suffix? thing ":")) (display "\t") (void)) (displayln thing)) #:exists 'append))

(define (emit-string thing [writer write-to-asm])
  (writer thing))

(define (emit thing) (emit-string thing))

(define (global-prelude [emitter emit-string])
  (emitter "	.text
	.p2align 4,,15
	.globl _scheme_entry
_scheme_entry:
	movq %rdi, %r15"))
