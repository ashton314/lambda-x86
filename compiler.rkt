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

    ;; Labels
    [(node/labels _ bindings body)
     (compile-labels stack env bindings body)]

    ;; Function calls
    [(node/app _ (node/var _ func-name) args)
     (compile-application stack env func-name args)]
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
     (emit (movq (reg 'ret-val) (stack stack-bottom)))
     (compile-ast (cadr args) (- stack-bottom wordsize) env)
     (emit (movq (reg 'ret-val) (heap wordsize)))
     (emit (movq (stack stack-bottom) (reg 'ret-val)))
     (emit (movq (reg 'ret-val) (heap 0)))
     (emit (movq (reg 'heap) (reg 'ret-val)))
     (emit (orq (raw-immediate 1) (reg 'ret-val)))                 ; tag our return value as pointing to a pair
     (emit (addq (raw-immediate (* 2 wordsize)) (reg 'heap)))]

    ['car
     (compile-ast (car args) stack-bottom env)
     (emit (movq (mem #:offset -1 #:reg-b (reg 'ret-val)) (reg 'ret-val)))]

    ['cdr
     (compile-ast (car args) stack-bottom env)
     (emit (movq (mem #:offset 7 #:reg-b (reg 'ret-val)) (reg 'ret-val)))]

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

(define (compile-labels stack-bottom env bindings body)
  (let ([defs-end (gensym 'definition_end)])
    (emit (jmp defs-end))
    (let-values ([(new-stack new-env) (compile-bindings stack-bottom env bindings)])
      (emit (label defs-end))
      (compile-ast body new-stack new-env))))

(define (compile-bindings stack-bottom env bindings)
  (if (null? bindings)
      (values stack-bottom env)
      (let* ([bind (car bindings)]
             [func-label (function-label (node/lvar-name bind))]
             [new-env (env/extend env (node/lvar-name bind) func-label)]
             [body-stack-start (- (* wordsize (+ 1 (length (node/lvar-params bind)))))])
        (emit (label func-label))
        (let ([body-env (for/fold ([body-env new-env])
                                  ([i (in-naturals 1)]
                                   [p (node/lvar-params bind)])
                          (env/extend body-env p (stack (- (* wordsize i)))))])
          (compile-ast (node/lvar-body bind) body-stack-start body-env)
          (emit (ret)))
        (compile-bindings stack-bottom new-env (cdr bindings)))))

(define (compile-application stack-bottom env func args)
  ;; Compile arguments
  (for ([arg args]
        [i (in-naturals 1)])
    (compile-ast arg (- stack-bottom (* wordsize (+ 2 (length args)))) env)
    (emit (movq (reg 'ret-val) (stack (- stack-bottom (* wordsize i))))))

  ;; Emit call
  (emit (addq (raw-immediate (+ wordsize stack-bottom)) (reg 'stack)))
  (emit (call (env/lookup env func)))
  (emit (subq (raw-immediate (+ wordsize stack-bottom)) (reg 'stack))))

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

  ;; Cons, car, cdr
  (check-equal? (crc '(cons 1 2)) "(1 . 2)")
  (check-equal? (crc '(cons 1 (+ 1 2))) "(1 . 3)")
  (check-equal? (crc '(cons (* 2 3) (+ 1 2))) "(6 . 3)")
  (check-equal? (crc '(let ((foo (cons (* 2 3) (+ 1 7)))) (cons foo 1))) "((6 . 8) . 1)")
  (check-equal? (crc '(let ((foo (cons (* 2 3) (+ 1 7)))) (car foo))) "6")
  (check-equal? (crc '(let ((foo (cons (* 2 3) (+ 1 7)))) (cdr foo))) "8")
  (check-equal? (crc '(let ((foo (cons (* 2 3) (+ 1 7)))) (car (cons foo 1)))) "(6 . 8)")
  (check-equal? (crc '(let ((foo (cons (* 2 3) (+ 1 7)))) (cdr (cons foo 1)))) "1")
  (check-equal? (crc '(cons 1 (cons 2 (cons 3 (cons 4 5))))) "(1 . (2 . (3 . (4 . 5))))")

  ;; Functions
  (check-equal? (crc '(labels ((f (code (n) () (+ n 1)))) (app f 3))) "4")
  (check-equal? (crc '(labels ((f (code (a b) () (+ (* a 2) (* 2 b))))) (app f 2 3))) "10")
  (check-equal? (crc '(labels ((f (code (n) () (if (zero? n) 1 (* n (app f (- n 1))))))) (app f 5))) "120")
  (check-equal? (crc '(labels ((f (code (n acc) () (if (zero? n) acc (app f (- n 1) (* n acc)))))) (app f 5 1))) "120")

  (check-equal? (crc '(labels ((f (code (a b) () (+ a (* 2 b))))) (app f 2 3))) "8")
  (check-equal? (crc '(labels ((f (code (a b) () (+ a (* 2 b))))) (let ((a 2) (b 3)) (app f a b)))) "8")
  (check-equal? (crc '(labels ((f (code (a b) () (+ a (* 2 b))))) (let ((a 2) (b 3)) (cons (app f a b) a)))) "(8 . 2)")
  (check-equal? (crc '(labels ((f (code (a b) () (+ a (* 2 b))))) (let ((a 2) (b 3)) (cons (app f a b) b)))) "(8 . 3)")
  (check-equal? (crc '(labels ((f (code (a b) () (+ a (* 2 b))))) (let ((a 2) (b 3)) (cons a (app f a b))))) "(2 . 8)")
  (check-equal? (crc '(labels ((f (code (a b) () (+ a (* 2 b))))) (let ((a 2) (b 3)) (cons b (app f a b))))) "(3 . 8)")

  (check-equal?
   (crc '(labels ((f1 (code (n) () (if (zero? n) 1 (* n (app f1 (- n 1))))))
                  (f2 (code (n) () (if (zero? (- n 1)) n (* n (app f2 (- n 1))))))
                  (f3 (code (n acc) () (if (zero? n) acc (app f3 (- n 1) (* acc n)))))
                  (f4 (code (acc n) () (if (zero? n) acc (app f4 (* acc n) (- n 1)))))
                  (f5 (code (n) () (app f3 n 1))))

                 (let ((r-f1 (app f1 5))
                       (r-f2 (app f2 5))
                       (r-f3 (app f3 5 1))
                       (r-f4 (app f4 1 5))
                       (r-f5 (app f5 5)))
                   (cons
                    (cons (app f5 (- (app f5 3) 1))
                          (cons
                           (cons r-f1 r-f2) r-f3))
                    (cons (* 12 (+ 2 8))
                          (cons r-f4
                                (cons (let ((x 4)) (app f2 (+ x 1)))
                                      r-f5)))))))
   "((120 . ((120 . 120) . 120)) . (120 . (120 . (120 . 120))))")
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
  (emitter ".text
	.p2align 4,,15
	.globl _scheme_entry
_scheme_entry:
	movq %rdi, %r15"))
