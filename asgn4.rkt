#lang typed/racket

;; Define data structures for the abstract syntax tree (AST)
(require typed/rackunit)
(struct IdC ([id : Symbol]) #:transparent)
(define-type FundefC (U FunC))
(struct FunC ([name : Symbol] [arg : Symbol] [body : ExprC])#:transparent)

(define-type ExprC (U NumC BinopC leq0? IdC FunAppC))
(struct BinopC ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct NumC ([n : Real]) #:transparent)
(struct leq0? ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct FunAppC ([fun : Symbol] [arg : ExprC]) #:transparent)

;; hash-table for BinopC, converts binary operators to their corresponding
;; racket operation
(define ops
  (hash
   '+ +
   '* *
   '- -
   '/ /))

(define badsyms
  (hash
   'def #f
   'leq0? #f
   'else #f
   'then #f
   '= #f))

;; ValidSymbol? checks if a symbol is valid for use in the AST
(define (ValidSymbol? [sym : Symbol]) : Boolean
  (cond
    [(hash-has-key? ops sym) #f]
    [(hash-has-key? badsyms sym) #f]
    [else #t]))

;; parse-prog converts a list of S-expressions into a list of FundefC
(define (parse-prog [s : Sexp]) : (Listof FundefC)
  (map parse-fundef (cast s (Listof Sexp))))

;; top-interp interprets an S-expression as a program and returns the result
(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))

;; interp-fns interprets a list of FundefC and returns the result of the main function
(define (interp-fns [funs : (Listof FundefC)]) : Real
  (define main (lookup-fun 'main funs))
  (define init (NumC 0))
  (define main-body-substituted (subst 'init init (FunC-body main)))
  (interp main-body-substituted funs))

;; lookup-fun finds a function definition by its name in a list of FundefC
(define (lookup-fun (name : Symbol) (funs : (Listof FundefC))) : FundefC
  (match funs
    [(list) (error 'interp "VVQS: function not found ~e" name)]
    [(cons f rest)
     (match f
       [(FunC fname _ _ )
        (if (symbol=? name fname) f (lookup-fun name rest))])]))

;; main VVQS parsing function
;; parse converts an S-expression into an ExprC (AST)
(define (parse [expr : Sexp]) : ExprC
  (match expr
    [(? real? n) (NumC n)]
    [(list (? symbol? s) l r) (if (hash-has-key? ops s)
                                  (BinopC s (parse l) (parse r))
                                  (error 'parse "VVQS: illegal operator ~e" s))]
    [(list 'leq0? test 'then then 'else else)
     (leq0? (parse test) (parse then) (parse else))]
    [(? symbol? (? ValidSymbol? id)) (IdC id)]
    [(list (? symbol? (? ValidSymbol? f)) arg) (FunAppC f (parse arg))]
    [other (error 'parse "VVQS: illegal expression: ~e" other)]))

;; parse-fundef converts an S-expression into a FundefC (function definition)
(define (parse-fundef [s : Sexp]) : FundefC
  (match s
    [(list 'def (list (? symbol? (? ValidSymbol? id))
                      (? symbol? (? ValidSymbol? arg))) '= exp)
     (FunC id arg (parse exp))]
    [other (error 'parse-fundef "VVQS: illegal function ~e" s)]))

;; interp consumes an abstract syntax tree to produce an answer
;; in the context of a list of FundefC
(define (interp [exp : ExprC] [funs : (Listof FundefC)]) : Real
  (match exp
    [(NumC n) n]
    [(BinopC o l r)
     ((hash-ref ops o) (interp l funs) (interp r funs))]
    [(leq0? test then else) (if (<= (interp test funs) 0)
                                (interp then funs)
                                (interp else funs))]
    [(IdC id) (error 'interp "VVQS: unbound identifier ~e" id)]
    [(FunAppC fun arg)
     (define fun-def (lookup-fun fun funs))
     (define arg-val (interp arg funs))
     (define substituted-body (subst (FunC-arg fun-def) (NumC arg-val) (FunC-body fun-def)))
     (interp substituted-body funs)]))

;; subst substitutes an ExprC for a Symbol in another ExprC
(define (subst (x : Symbol) (v : ExprC) (e : ExprC)) : ExprC
  (match e
    [(NumC _) e]
    [(IdC id) (if (symbol=? x id) v e)]
    [(BinopC o l r) (BinopC o (subst x v l) (subst x v r))]
    [(leq0? test then else) (leq0? (subst x v test) (subst x v then) (subst x v else))]
    [(FunAppC fun arg) (FunAppC fun (subst x v arg))]))

