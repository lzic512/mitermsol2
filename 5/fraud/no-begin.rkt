#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

;; Expr -> Expr
(define (no-begin e)
  (match e
    [(Lit l)            (Lit l)]
    [(Var x)            (Var x)]
    [(Prim0 p)          (Prim0 p)]
    [(Prim1 p e1)       (Prim1 p e1)]
    [(Prim2 p e1 e2)    (Prim2 p e1 e2)]
    [(If e1 e2 e3)      (If e1 e2 e3)]
    [(Begin e1 e2)      (Let (Lit 1) e1 e2)]
    [(Let x e1 e2)      (Let x e1 e2)])
    ;; TODO
  )

