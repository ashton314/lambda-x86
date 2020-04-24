#lang racket
;(require racket/match racket/pretty)
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
  (compile-expr expr '() (- wordsize))
  (emit-string (ret)))

(define (compile-expr expr env stack-bottom [emit emit-string])
  (match expr

    [(node/immediate _ num)
     (emit (movq (immediate num) (reg 'ret-val)))]

    [(node/prim type 'cons 2 args)
     (emit (label (gensym 'start_cons)))
     (compile-expr (cadr args) env stack-bottom emit)                ; compile the cdr
     (let-values ([(the-car new-stack) (push-stack (reg 'ret-val) stack-bottom emit)])
       (compile-expr (car args) env (- new-stack wordsize) emit)     ; compile the car
       (emit (movq (reg 'ret-val) (heap 0)))                         ; copy car to free pointer
       (emit (movq the-car (reg 'ret-val)))                          ; copy the cdr to next area
       (emit (movq (reg 'ret-val) (heap wordsize)))
       (emit (movq (reg 'heap) (reg 'ret-val)))
       (emit (orq (raw-immediate 1) (reg 'ret-val)))                 ; tag our return value as pointing to a pair
       (emit (addq (raw-immediate (* 2 wordsize)) (reg 'heap)))
       (emit (label (gensym 'end_cons))))]

    [(node/prim type name 2 args)
     ;; These are in a funky order because `-` is not communative
     (compile-expr (cadr args) env stack-bottom emit)
     (let-values ([(arg1 new-stack) (push-stack (reg 'ret-val) stack-bottom emit)])
       (compile-expr (car args) env new-stack emit)
       (emit ((prim-bin-op name) arg1 (reg 'ret-val))))]

    [(node/if type condition t-case f-case)
     (let ([l0 (gensym 'false_branch)]
           [l1 (gensym 'cond_end)])
       (compile-expr condition env stack-bottom emit)
       (emit (cmpq (immediate #f) (reg 'ret-val)))
       (emit (je l0))                   ; jump to the false branch
       (compile-expr t-case env stack-bottom emit)
       (emit (jmp l1))
       (emit (label l0))
       (compile-expr f-case env stack-bottom emit)
       (emit (label l1)))]

    [(node/app type (node/var _ func-name) args)                     ; TODO: look up the function arity
     (emit (label (gensym 'start_app)))

     ;; set stack back to zero so we can push
     ;; we do this in case we've done some "fake" pushing
     (emit (addq (immediate stack-bottom) (reg 'stack)))
     ;; We can now pretend the stack is at 0; we own the stack!

     ;; Store params
     (for ([param '(param-1 param-2 param-3 param-4)])
       (emit (push (reg param))))

     ;; Move arguments into slots on the stack
     (for ([arg args]                                                ; I love me some list comprehensions
           [idx (in-naturals)]
           [p-idx '(param-1 param-2 param-3 param-4)])
       (emit (label (gensym (format "arg_~a" idx))))
       (compile-expr arg env (- stack-bottom (* wordsize 5)))        ; FIXME: make this be based off the number of arguments
       (emit (movq (reg 'ret-val) (stack (- stack-bottom (* wordsize idx)))))
       (emit (label (gensym (format "arg_~a_done" idx)))))

     ;; All arguments computed; move them off the stack and into registers
     (for ([p-idx '(param-1 param-2 param-3 param-4)]
           [idx (in-naturals)])
       (emit (movq (stack (- stack-bottom (* wordsize idx))) (reg p-idx))))

     ;; Normalize stack before call: we might have manually allocated
     ;; some values on the stack, and we want to preserve them

     ;; TODO: in the future, the `push-stack` routine should probably
     ;; push the stack and then return the absolute address to the
     ;; value---I think I can use the load-effective-addr instruction
     ;; for this

     (emit (addq (immediate (- stack-bottom wordsize)) (reg 'stack)))

     ;; Call
     (emit (call (env/var-info env func-name)))

     ;; Restore old stack
     (emit (subq (immediate (- stack-bottom wordsize)) (reg 'stack)))

     ;; Restore parameters
     (for ([param '(param-4 param-3 param-2 param-1)])
       (emit (pop (reg param))))

     ;; Restore stack pointer to it's old value (prior to saving the arguments)
     (emit (subq (immediate stack-bottom) (reg 'stack)))
     ]

    [(node/labels _ lvars body)
     (compile-labels lvars body env stack-bottom emit)]

    [(node/lambda type params body)
     (compile-lambda type params body env stack-bottom emit)]

    [(node/let type bindings body)
     (compile-let type bindings body env stack-bottom emit)]

    [(node/var type name)
     (emit (movq (if (symbol? (env/var-info env name))               ; If I've got a symbol, it's a register name;
                     (reg (env/var-info env name))                   ; numbers mean an offset from the current stack
                     (stack (or (env/var-info env name) (error 'undefined-variable))))
                 (reg 'ret-val)))]

    ))

(define (compile-let type bindings body env stack-bottom [emit emit-string])
  (if (null? bindings)
      ;; If no more bindings to compile, compile the let body
      (compile-expr body env stack-bottom emit)
      (let ([b (car bindings)])
        ;; Ok, we've got a binding. Let's compile it's value
        (compile-expr (node/let-binding-value b) env stack-bottom emit)
        ;; Now we're going to move that result into the bottom of the stack
        (emit (movq (reg 'ret-val) (stack stack-bottom)))
        ;; Recur: compile the rest of the bindings: increment the
        ;; stack, and extend the env to point to the stack position we
        ;; just moved the value of this binding's variable to
        (compile-let type (cdr bindings) body
                     (env/extend env
                                 (node/let-binding-variable b)
                                 stack-bottom)
                     (- stack-bottom wordsize)
                     emit))))

(define (compile-labels defs def-body env stack-bottom [emit emit-string])
  (let ([end-label (fresh-label)])
    (emit (jmp end-label))
    (define (compile-labels-loop defs def-body env stack-bottom)
      (if (null? defs)
          ;; No more defs? Compile the body where the function calls will be
          (begin
            (emit (label end-label))
            (compile-expr def-body env stack-bottom emit))
          (match defs
            [(list (node/lvar _ name params body) rest-defs ...)                       ; Pull out the first binding
             (let* ([new-label (function-label name)]                                  ; Create a new label for this function
                    [new-env (for/fold ([new-env (env/extend env name new-label)])     ; Extend the env: map the param to a location on the stack
                                       ([i '(param-1 param-2 param-3 param-4 param-5 param-6 param-7 param-8)]
                                        [p params])
                               (env/extend new-env p i))])
               (emit (label new-label))                                                ; Emit the function label
               ;; Warning: maybe an off-by-one error
               (compile-expr body new-env stack-bottom emit)                              ; Compile the body of function in this lable
               (emit (ret))
               (compile-labels-loop rest-defs def-body new-env stack-bottom))])))

    (compile-labels-loop defs def-body env stack-bottom)))

(define (compile-lambda type params body env stack-bottom [emit emit-string])
  (error "Darn. This is a hard problem."))

(define (push-stack var-loc stack-bottom [emit emit-string])
  ;; Emit code needed to push a variable onto the stack, and return
  ;; the new stack bottom and the location the variable was at

  ;; We assume stack-bottom is writeable; that means the caller will
  ;; have to ensure we never call 0(%esp), because that would
  ;; overwrite the return value. We place this burden on the caller so
  ;; that we can call this function sequentially without wasting space
  ;; on the stack.
  (emit (movq var-loc (stack stack-bottom)))
  (values (stack stack-bottom) (- stack-bottom wordsize)))

(define (env/new) '())

;; Returns the info associated with a variable from the environment
(define (env/var-info env var)
  (match env
    [(list) #f]                   ; Not in env
    [(cons (cons v t) rst)
     #:when (eq? v var)
     t]
    [(cons _ rst) (env/var-info rst var)]))

;; Extend a env with a value
(define (env/extend env var info)
  (cons (cons var info) env))

[module+ test
  (let ([ctx (env/extend (env/new) 'foo 'integer)])
    (check-eq? (env/var-info ctx 'foo) 'integer)
    (check-eq? (env/var-info (env/extend ctx 'bar 'string) 'foo) 'integer)
    (check-eq? (env/var-info (env/extend ctx 'bar 'string) 'bar) 'string))]

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
  (println (list 'emit: thing))
  (with-output-to-file asm-file (λ () (if (not (string-suffix? thing ":")) (display "\t") (void)) (displayln thing)) #:exists 'append))

(define (emit-string thing [writer write-to-asm])
  (writer thing))

(define (global-prelude [emitter emit-string])
  (emitter "	.text
        .p2align 4,,15
	.globl _scheme_entry
_scheme_entry:
        movq %rdi, %r15
"))

[module+ test
  (check-eqv? (global-prelude (λ (x) x)) "	.text
        .p2align 4,,15
	.globl _scheme_entry
_scheme_entry:
        movq %rdi, %r15
")]

;; Torture-test

[module+ torture-test
  ;; This should be (120 . 5)
  (compile (parse '(labels ((f2 (code (n) (if (= n 1) n (* n (app f2 (- n 1))))))) (cons (let ((x 4)) (app f2 (+ x 1))) 5))))
  (compile (parse '(labels ((f2 (code (n) (if (= n 1) n (* n (app f2 (- n 1))))))) (cons (let ((x 5)) (app f2 x)) 5))))

  ;; This should be ((120 . ((120 . 120) . 120)) . (120 . (120 . (120 . 120))))
  (compile (parse '(labels ((f1 (code (n) (if (= n 0) 1 (* n (app f1 (- n 1))))))
                   (f2 (code (n) (if (= n 1) n (* n (app f2 (- n 1))))))
                   (f3 (code (n acc) (if (= n 0) acc (app f3 (- n 1) (* acc n)))))
                   (f4 (code (acc n) (if (= 0 n) acc (app f4 (* acc n) (- n 1)))))
                   (f5 (code (n) (app f3 n 1))))

                  (let ((r-f1 (app f1 5))
                        (r-f2 (app f2 5))
                        (r-f3 (app f3 5 1))
                        (r-f4 (app f4 1 5))
                        (r-f5 (app f5 5)))
                    (cons (cons (app f5 (- (app f5 3) 1)) (cons (cons r-f1 r-f2) r-f3)) (cons (* 12 (+ 2 8)) (cons r-f4 (cons
                                                                                                                         (let ((x 4)) (app f2 (+ x 1)))
                                                                                                                         r-f5))))))))
  
  ]
