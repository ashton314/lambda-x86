#lang racket/base
(require racket/match)

(provide cps)

(define (cps pr)
  (define (lambda? sym) (or (eq? 'λ sym) (eq? 'lambda sym)))
  (define (param-list? xs)
    (and (list? xs) (andmap symbol? xs)))
  (define (apply-κ q x)
    `(kapp ,q ,x))
  (define (cps-λ xs e)
    (let ([k (gensym 'k)])
      `(λ (,k . ,xs) ,(cps e k))))
  (define (cps-μ f xs e)
    (let ([k (gensym 'k)])
      `(μ (,k ,f . ,xs) ,(cps e k))))
  (define (atomize* es κ)
    (match es
      [(list) (κ (list))]
      [(cons e es) (atomize e (λ (a) (atomize* es (λ (as) (κ (cons a as))))))]))
  (define (atomize e κ)
    (match e
      [(? exact-integer? n)
       (κ n)]
      [(? string? s)
       (κ s)]
      [(? symbol? x)
       (κ x)]
      [`(,(? lambda? _) ,(? param-list? xs) ,e)
       (κ (cps-λ xs e))]
      [`(μ (,(? symbol? f) . ,(? param-list? xs)) ,e)
       (κ (cps-μ f xs e))]
      [`(app ,f . ,es)
       (let ([v (gensym 'v)])
         (cps `(app ,f . ,es) `(κ (,v) ,(κ v))))]
      [`(prim ,+ ,e₀ ,e₁)
       (atomize e₀ (λ (a₀) (atomize e₁ (λ (a₁) (κ `(prim ,+ ,a₀ ,a₁))))))]
      [`(if ,r ,e₀ ,e₁ ,c ,a)
       (let ([v (gensym 'v)])
         (cps `(if ,r ,e₀ ,e₁ ,c ,a) `(κ (,v) ,(κ v))))]))
  (define (cps e q)
    (match e
      [(? symbol? x)
       (apply-κ q x)]
      [(? exact-integer? n)
       (apply-κ q n)]
      [`(let ([,x ,e₀]) ,e₁)
       (cps e₀ `(κ (,x) ,(cps e₁ q)))]
      [`(λ ,(? param-list? xs) ,e)
       (atomize `(λ ,xs ,e) (λ (λxs.e) (apply-κ q λxs.e)))]
      [`(μ (,(? symbol? f) . ,(? param-list? xs))  ,e)
       (atomize `(μ (,f . ,xs) ,e) (λ (μfxs.e) (apply-κ q μfxs.e)))]
      [`(app ,f . ,es)
       (atomize f (λ (f) (atomize* es (λ (as) `(app ,f ,q . ,as)))))]
      [`(prim ,op ,e₀ ,e₁)
       (atomize e₀ (λ (e₀) (atomize e₁ (λ (e₁) (apply-κ q `(prim ,op ,e₀ ,e₁))))))]
      [`(if ,r ,e₀ ,e₁ ,c ,a)
       (atomize
        e₀
        (λ (a₀)
          (atomize
           e₁
           (λ (a₁)
             (let ([with-k (λ (k) `(if ,r ,a₀ ,a₁ ,(cps c k) ,(cps a k)))])
               (match q
                 [(? symbol? k)
                  (with-k k)]
                 [`(κ (,x) ,e)
                  (let ([k (gensym 'k)])
                    `(letk ,k (κ (,x) ,e) ,(with-k k)))]))))))]))
  (match pr
    [`(,(? lambda? _) ,(? param-list? xs) ,e)
     (cps-λ xs e)]))
