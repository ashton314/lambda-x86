#lang racket/base
;; (provide node/app node/prim node/foreign node/var node/lambda node/immediate type/arrow)
(require racket/generic)
(provide (all-defined-out))

(module+ test
  (require rackunit))

(define-generics node
  [typeof node]
  [set-type! node new-type])

;; Known AST node types:
;;  - app (application)
;;  - prim (primitive operation)
;;  - foreign (foreign function---look up in db for definition)
;;  - var (variable reference)
;;  - lambda (lambda function)
;;
;;
;; Observable types: (these are the names used in the program)
;;  - integer
;;  - string
;;  - boolean
;;
;; Opaque types:
;;  - (function (arg-type ...) return-type)

;; Function application
(struct node/app
  (type func args)
  #:mutable #:transparent
  #:methods gen:node
  [(define (typeof node) (node/app-type node))
   (define (set-type! node new-type) (set-node/app-type! node new-type))])
  
;; Primitive operation
(struct node/prim
  (type name arity args)
  #:mutable #:transparent
  #:methods gen:node
  [(define (typeof node) (node/prim-type node))
   (define (set-type! node new-type) (set-node/prim-type! node new-type))])

;; Foreign function call
(struct node/foreign
  (type name arity args)
  #:mutable #:transparent
  #:methods gen:node
  [(define (typeof node) (node/foreign-type node))
   (define (set-type! node new-type) (set-node/foreign-type! node new-type))])

;; Variable reference
(struct node/var
  (type name)
  #:mutable #:transparent
  #:methods gen:node
  [(define (typeof node) (node/var-type node))
   (define (set-type! node new-type) (set-node/var-type! node new-type))])

(struct node/if
  (type condition t-case f-case)
  #:mutable #:transparent
  #:methods gen:node
  [(define (typeof node) (node/if-type node))
   (define (set-type! node new-type) (set-node/if-type! node new-type))])


;; A lambda
(struct node/lambda
  (type params body)
  #:mutable #:transparent
  #:methods gen:node
  [(define (typeof node) (node/lambda-type node))
   (define (set-type! node new-type) (set-node/lambda-type! node new-type))])

;; Immediate values: integers, string litterals, etc.
(struct node/immediate
  (type value)
  #:mutable #:transparent
  #:methods gen:node
  [(define (typeof node) (node/immediate-type node))
   (define (set-type! node new-type) (set-node/immediate-type! node new-type))])

;; Type annotation
(struct node/ann
  (type value)
  #:mutable #:transparent
  #:methods gen:node
  [(define (typeof node) (node/ann-type node))
   (define (set-type! node new-type) (set-node/ann-type! node new-type))])

(struct type/observable
  (name))

;; Arrow type
(struct type/arrow
  (params body [name #:auto])
  #:mutable #:transparent #:auto-value 'arrow)
