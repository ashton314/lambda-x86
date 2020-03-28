#lang racket/base
(require racket/match racket/pretty)
(require "cps_converter.rkt")
(require "ast.rkt")

(module+ test
  (require rackunit))

(provide parse typecheck convert-parse)

;; Query Language Parser

(define (lambda? sym) (and (member sym '(lambda λ)) (symbol? sym)))
(define (bin-int-prim? sym) (and (member sym '(+ - * / =)) (symbol? sym)))

;; Convert into CPS and parse
(define (convert-parse expr)
  (parse (cps expr)))

;; Entry point. We assume that the expression has been alphetized and is in CPS.
(define (parse expr)
  (match expr
    ;; Lambda
    [`(,(? lambda? _) ,xs (-> ,param-types ... ,body-type) ,body)
     (node/lambda (type/arrow param-types body-type) xs (parse body))]

    [`(,(? lambda? _) ,xs ,body)
     (node/lambda 'unknown xs (parse body))]

    ;; Type annotation
    ;; TODO: check to make sure this annotation is actually valid
    [`(ann ,e ,t)
     (node/ann t (parse e))]

    [`(if ,condition ,t-arm ,f-arm)
     (node/if 'unknown (parse condition) (parse t-arm) (parse f-arm))]

    [`(let ([,(? symbol? var) ,val] ...)
        ,body)
     (node/let 'unknown
               (map (λ (var val) (node/let-binding 'unknown var (parse val))) var val)
               (parse body))]

    ;; Function application
    [`(app ,func . ,args)
     ;; TODO: What do I do with cont?
     ;; Do I parse it? I don't think so… it doesn't have a value because it diverges, right?
     (let* ([func-node (parse func)]
            [arg-nodes (map (λ (arg) (parse arg)) args)])
       (node/app 'unknown func-node arg-nodes))]

    ;; For type-checking the continuation, I think we can synthesize
    ;; the type of the argument to the contiuation (i.e. the value
    ;; that is being passed on) and then annotate that value as it
    ;; gets passed down the tree.
    [`(kapp ,cont . ,args)
     (let* ([cont-node (parse cont)]
            [arg-nodes (map (λ (arg) (parse arg)) args)])
       (node/app 'void cont-node arg-nodes))]

    ;; Integer primitives of two arguments
    [`(,(? bin-int-prim? op) ,x1 ,x2)
     (let* ([x1-node (parse x1)]
            [x2-node (parse x2)])
       (node/prim 'integer op 2 (list x1-node x2-node)))]

    ;; Symbols (i.e. variables)
    [(? symbol? x)
     (node/var 'unknown x)]

    ;; Primitives
    [(? exact-integer? num) (node/immediate 'integer num)]
    [(? string? str) (node/immediate 'string str)]
    [(? boolean? bool) (node/immediate 'boolean bool)]
    ))

[module+ test
  (check-equal? (parse '(let ([x 1] [y 2]) (+ x y)))
                (node/let 'unknown
                          (list (node/let-binding 'unknown 'x (node/immediate 'integer 1))
                                (node/let-binding 'unknown 'y (node/immediate 'integer 2)))
                          (node/prim 'integer '+ 2 (list (node/var 'unknown 'x) (node/var 'unknown 'y)))))
  ]

;; Set the type of a node to `type`. If there's already a type
;; associated with this node and it doesn't match, throw a type error.
(define (resolve-type! node type)
  ;; TODO: make this handle subtyping
  (if (member (typeof node) (list 'unknown type))
      (begin (set-type! node type) node)
      (error 'typecheck "type violation: expected ~s, found type ~s at ~s" type (typeof node) node)))

;; TODO: create a better system for types and type checking
(define (polymorphic? t)
  ;; Is this a polymorphic type?
  (member t '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
(define (type/<=? t1 t2)
  (or (eq? t1 t2) (polymorphic? t2)))

[module+ test
  (check-equal? (parse '(lambda (x) x))
                (node/lambda 'unknown '(x) (node/var 'unknown 'x)))
  (check-equal? (parse '(lambda (x) (app (lambda (y) y) 42)))
                (node/lambda 'unknown '(x) (node/app 'unknown (node/lambda 'unknown '(y) (node/var 'unknown 'y)) (list (node/immediate 'integer 42)))))

  (check-exn exn:fail? (lambda () (resolve-type! (node/app 'foo 'bar 'baz) 'zoop)))
  (check-equal? (resolve-type! (node/app 'unknown 'bar 'baz) 'foo) (node/app 'foo 'bar 'baz))

  (check-equal? (parse '(+ x y)) (node/prim 'integer '+ 2 (list (node/var 'unknown 'x) (node/var 'unknown 'y))))
  ]

(define (context/new) '())

;; Returns the type of a variable from the context
(define (context/var-type context var)
  (match context
    [(list) #f]                   ; Not in context
    [(cons (cons v t) rst)
     #:when (eq? v var)
     t]
    [(cons _ rst) (context/var-type rst var)]))

;; Extend a context with a value
(define (context/extend context var type)
  (cons (cons var type) context))

[module+ test
  (let ([ctx (context/extend (context/new) 'foo 'integer)])
    (check-eq? (context/var-type ctx 'foo) 'integer)
    (check-eq? (context/var-type (context/extend ctx 'bar 'string) 'foo) 'integer)
    (check-eq? (context/var-type (context/extend ctx 'bar 'string) 'bar) 'string))]

(define (typecheck ast env)
  ;; Check an expression `expr` against type `type` in `context`
  ;; Returns expr with type set (if it doesn't fail)
  (define (check expr type context)
    (match expr
      [(node/immediate typ val)
       (unless (type/<=? typ type) (error 'typecheck "type violation: expected ~s, found type ~s at ~s" type typ expr))
       (node/immediate typ val)]

      [(node/lambda (type/arrow parms-types ret-type _) parms body)
       (let* ([lambda-ctx (foldl (λ (p pt ctx) (context/extend ctx p pt)) context parms parms-types)]
              [checked-body (check body ret-type lambda-ctx)])
         (node/lambda (type/arrow parms-types ret-type) parms checked-body))
       ]

      ;; TODO: maybe add an arm in here for lambdas of unknown types
      ;; that we can derive a polymorphic type for

      [(node/prim typ name arity args)
       (unless (type/<=? typ type) (error 'typecheck "type violation: expected ~s but primitive returns ~s at ~s" type typ expr))
        ;; TODO: add more flexible checking here
        (if (bin-int-prim? name)
            (let ([a1 (check (car args) 'integer context)]
                  [a2 (check (cadr args) 'integer context)])
              (node/prim typ name arity (list a1 a2)))
            (node/prim typ name arity args))
        ]

      [(node/var 'unknown name)
       (let ([var-type (context/var-type context name)])
         (unless (type/<=? var-type type) (error 'typecheck "type violation: expected ~s, variable ~s has type ~s at ~s" type name var-type expr))
         (node/var var-type name))]

      [(node/var typ name)
       (let ([env-type (context/var-type context name)])
         (unless (type/<=? typ env-type) (error 'typecheck "type violation: variable ~s was inferred to have type ~s, but has type ~s in env" name typ env-type))
         (node/var typ name))]

      ))

  ;; Synthesize a type for `expr` under `context`
  ;; Returns the expr with the type set (if it doesn't fail)
  ;; TODO: do I also need to return the context? I don't think so... (variable scope should be preserved)
  (define (synth expr context)
    (match expr
      [(node/immediate typ val)
       (node/immediate typ val)]

      [(node/prim typ name arity args)
        ;; TODO: add more flexible checking here
        (if (bin-int-prim? name)
            (let ([a1 (check (car args) 'integer context)]
                  [a2 (check (cadr args) 'integer context)])
              (node/prim typ name arity (list a1 a2)))
            (node/prim typ name arity args))
        ]

      [(node/var typ name)
       (node/var (or (context/var-type context name) typ) name)]

      ;; TODO: is there something I can do here to make polymorphic function types?
      [(node/lambda type parms body)
       (check expr type context)]        ; This is just a little glue to make sure that we can apply annotated lambdas

      [(node/app 'unknown fun args)
       (let* ([typed-node (synth fun context)]
              [body-type (type/arrow-body (typeof typed-node))]
              [param-types (type/arrow-params (typeof typed-node))])
         (check typed-node (typeof typed-node) context)
         (let ([params-with-types (map (λ (p t) (check p t context)) args param-types)])
           (node/app body-type typed-node params-with-types)))
       ]))

  (synth ast env))
