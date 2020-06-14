;;
;;
;;  Ersatz evaluation.
;;
;;  Based on the Ocaml Jingoo library, which is in turn based on the
;;  Python Jinja2 library.
;;
;; Copyright 2012-2014 Ivan Raikov
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.
;;

(define (eval-expr env ctx x)
  (cases texpr x
	 (LiteralExpr (x) x)
	 (IdentExpr (name) 
		    (get-value ctx name))
	 (NotOpExpr (x)
		    (test-not (eval-expr env ctx x)))
	 (NegateOpExpr (x)
		       (op-negate (eval-expr env ctx x)))
	 (PlusOpExpr (l r)
		     (op-plus (eval-expr env ctx l)
			      (eval-expr env ctx r)))
	 (MinusOpExpr (l r)
		     (op-minus (eval-expr env ctx l)
			      (eval-expr env ctx r)))
	 (TimesOpExpr (l r)
		      (op-times (eval-expr env ctx l)
				(eval-expr env ctx r)))
	 (PowerOpExpr (l r)
		      (op-power (eval-expr env ctx l)
				(eval-expr env ctx r)))
	 (DivOpExpr (l r)
		      (op-div (eval-expr env ctx l)
			      (eval-expr env ctx r)))
	 (ModOpExpr (l r)
		      (op-mod (eval-expr env ctx l)
			      (eval-expr env ctx r)))
	 (AndOpExpr (l r)
		      (op-and (eval-expr env ctx l)
			      (eval-expr env ctx r)))
	 (OrOpExpr (l r)
		      (op-or (eval-expr env ctx l)
			     (eval-expr env ctx r)))
	 (EqEqOpExpr (l r)
		      (eq-eq (eval-expr env ctx l)
			     (eval-expr env ctx r)))
	 (NotEqOpExpr (l r)
		      (not-eq (eval-expr env ctx l)
			      (eval-expr env ctx r)))
	 (LtOpExpr (l r)
		   (op-lt (eval-expr env ctx l)
			  (eval-expr env ctx r)))
	 (GtOpExpr (l r)
		   (op-gt (eval-expr env ctx l)
			  (eval-expr env ctx r)))
	 (LtEqOpExpr (l r)
		     (op-lteq (eval-expr env ctx l)
			      (eval-expr env ctx r)))
	 (GtEqOpExpr (l r)
		     (op-gteq (eval-expr env ctx l)
			      (eval-expr env ctx r)))
	 (InOpExpr (l r)
		   (op-in (eval-expr env ctx l)
			  (eval-expr env ctx r)))
	 (ListExpr (lst)
		   (Tlist (map (lambda (x) (eval-expr env ctx x)) lst)))
	 (SetExpr (lst)
		  (Tset (map (lambda (x) (eval-expr env ctx x)) lst)))
	 (DotExpr (objexpr propexpr)
                  (cases texpr objexpr
                         (IdentExpr (name) 
                            (let ((prop (ident-expr->name propexpr)))
                              (tobj-lookup ctx name prop)))
                         (DotExpr (_ _) 
                                  (let ((v (eval-expr env ctx objexpr))
                                        (prop (ident-expr->name propexpr)))
                                    (tobjval-lookup v prop)))
                         (else (error 'eval-expr "invalid object dot expression" objexpr))))
	 (BracketExpr (objexpr propexpr)
                      (cases texpr objexpr
                             (IdentExpr (name) 
                                        (cases tvalue (eval-expr env ctx propexpr)
                                               (Tstr (s) (tobj-lookup ctx name (string->symbol s)))
                                               (else (error 'eval-expr "invalid property in bracket expression" 
                                                            propexpr))
                                               ))
                             (DotExpr (_ _) 
                                      (let ((v (eval-expr env ctx objexpr)))
                                        (cases tvalue (eval-expr env ctx propexpr)
                                               (Tstr (s) (tobjval-lookup v (string->symbol s)))
                                               (else (error 'eval-expr "invalid property in bracket expression" 
                                                            propexpr))
                                               )))
                             (else (error 'eval-expr "invalid object bracket expression" objexpr))))
	 (TestOpExpr (objexpr pred)
	     (cases texpr pred
		    (IdentExpr (predname)
		       (case predname 
			 ((defined)
			  (cases texpr objexpr
				 (IdentExpr (name)
                                            (test-defined ctx name))
				 (DotExpr (objexpr propexpr)
                                          (let ((name (ident-expr->name objexpr))
                                                (prop (ident-expr->name propexpr)))
                                            (test-obj-defined ctx name prop)))
				 (BracketExpr (objexpr propexpr)
                                              (let ((name (ident-expr->name objexpr))
                                                    (prop (ident-or-string-expr->name propexpr)))
                                                (test-obj-defined ctx name prop)))
				 (else (error 'eval-expr "invalid predicate object expression"
					      objexpr))
				 ))
			 ((undefined)
			  (cases texpr objexpr
				 (IdentExpr (name)
				    (test-undefined ctx name))
				 (DotExpr (objexpr propexpr)
 				    (let ((name (ident-expr->name objexpr))
					  (prop (ident-expr->name propexpr)))
				      (test-obj-undefined ctx name prop)))
				 (else (error 'eval-expr "invalid predicate object expression"
					      objexpr))
				 ))
			 ((none)
			  (cases texpr objexpr
				 (IdentExpr (name)
				    (test-none ctx name))
				 (else (error 'eval-expr "invalid predicate object expression"
					      objexpr))
				 ))
			 ((escaped)
			  (cases texpr objexpr
				 (IdentExpr (name)
				    (test-escaped ctx))
				 (else (error 'eval-expr "invalid predicate object expression"
					      objexpr))
				 ))
			 (else
			  (tfun-apply (eval-expr env ctx pred)
				      (list (eval-expr env ctx objexpr))))))

		    (else 
		     (tfun-apply (eval-expr env ctx pred)
				 (list (eval-expr env ctx objexpr))))))

	 (ObjExpr (exprs)
		  (Tobj (map (lambda (id.val)
			       (cases texpr (car id.val)
				      (IdentExpr (name)
						 (cons name (eval-expr env ctx (cdr id.val))))
				      (LiteralExpr (strval expr)
						   (cons (string->symbol (unbox-string strval))
							 (eval-expr env ctx (cdr id.val))))
				      (else (error 'eval-expr "invalid object expression" id.val))))
			     exprs)))

	 (ApplyExpr (opexpr argexprs)
		    (let ((opname (ident-expr->name/safe opexpr)))
		      (if (and opname (eq? 'eval opname))
			  (let* ((ctx (template-context-with-buffer
				       ctx (open-output-string)))
				 (stmts (statements-from-string env (->string (eval-expr env ctx (car argexprs)))))
				 (ctx (fold (lambda (s ctx) (eval-statement env ctx s)) ctx stmts)))
			    (Tstr (get-output-string (tmpl-ctx-buffer ctx))))
			  (let ((name (apply-name-of opexpr))
				(nargs (nargs-of env ctx argexprs))
				(kwargs (kwargs-of env ctx argexprs))
				(callable (eval-expr env ctx opexpr)))

			    (cases tvalue callable
				   (Tfun (fn)
					 (if (null? nargs)
					     (Tfun (lambda _ (fn argexprs kwargs)))
					     (tfun-apply callable nargs name: name kwargs: kwargs)))
				   (else
				    (let ((mac (get-macro ctx name)))
				      (if mac
					  (begin
					    (eval-macro1 env ctx name nargs kwargs mac)
					    (Tnull))
					  (Tnull))))
				   ))
			  )))
	 (else (error 'eval-expr "invalid expression" x))
	 ))


(define (apply-name-of fnexpr)
  (cases texpr fnexpr
	 (IdentExpr (name)  name)
	 (DotExpr (objexpr propexpr)
		  (let ((name (ident-expr->name objexpr))
			(prop (ident-expr->name propexpr)))
		    (string->symbol (sprintf "~A.~A" name prop))))
	 (ApplyExpr (expr args)
		    (apply-name-of expr))
	 (else (string->symbol "<lambda>"))
	 ))
			

(define (ident-names-of lst)
  (map ident-expr->name
       (filter (lambda (x)
		 (cases texpr x
		   (IdentExpr (x) #t)
		   (else #f)))
	       lst)))


(define (alias-names-of lst)
  (map (lambda (x)
	 (cases texpr x
		(IdentExpr (name)  (cons name name))
		(AliasExpr (e1 e2)
			   (let ((n1 (ident-expr->name e1))
				 (n2 (ident-expr->name e2)))
			     (cons n1 n2)))
		(else (error 'alias-names-of "invalid argument" x))))
       lst))
			   

(define (nargs-of env ctx args)
  (map (lambda (x) (eval-expr env ctx x))
       (filter (lambda (x) (cases texpr x (KeywordExpr (x y) #f) (else #t))) 
	       args)))


(define (kwargs-of env ctx args)
  (map (lambda (x) 
	 (cases texpr x
		(KeywordExpr (x y)
			     (let ((name (ident-expr->name x)))
			       (cons name (eval-expr env ctx y))))))
       (filter (lambda (x) (cases texpr x (KeywordExpr (x y) #t) (else #f))) 
	       args)))


(define (eval-macro1 env ctx name args kwargs mac)

  (let ((caller (get-macro ctx 'caller))
	(f (lambda (ctx stmts)
	     (fold (lambda (s ctx) 
		       (eval-statement env ctx s)) 
		   ctx stmts)))) 
    (eval-macro env ctx name args kwargs mac f caller: (and caller #t))))



(define (eval-statement env ctx stmt)

  (if (> (debug) 0)
      (fprintf (current-output-port)
               "ersatz: stmt = ~A~%" stmt))

  (cases tstmt stmt

	 (TextStatement (text)
	      (tvalue-output ctx (Tstr text) safe: #t)
	      ctx)

	 (ExpandStatement (expr)
	      (tvalue-output ctx (eval-expr env ctx expr)
			     autoescape: (tmpl-env-autoescape env)
			     safe: (safe-expr? expr))
	      ctx)

	 (SetStatement (setexpr expr)
	    (cases texpr setexpr
		   (SetExpr (ident-lst)
			      (let ((ctx1 
				     (bind-names ctx (ident-names-of ident-lst)
						 (eval-expr env ctx expr))))
				ctx1))
                   
                   (DotExpr (ns-expr prop-expr)
                            (let ((ns (ident-expr->name ns-expr))
                                  (prop (ident-expr->name prop-expr)))
                              (extend-namespace ctx ns prop (eval-expr env ctx expr))))
                            
		   (else (error 'eval-statement "invalid set expression" setexpr))))
         
         (NamespaceStatement (ns bind-exprs)
                             (let ((h (fold (lambda (bind-expr h)
                                              (let ((name (car bind-expr))
                                                    (expr (cdr bind-expr)))
                                              (cons `(,name . ,(eval-expr env ctx expr)) h)))
                                            '() bind-exprs)))
                               (let ((ctx1 (set-namespace ctx ns h)))
                                 (print "ctx1 = " ctx1)
                                 ctx1)))

	 (FilterStatement (nexpr stmts)
	    (let ((name (ident-expr->name nexpr)))
	      (let* ((ctx (set-filter ctx name))
		     (ctx (fold (lambda (s ctx) (eval-statement env ctx s)) 
				ctx stmts)))
		(pop-filter ctx)
		)))
	 
	 (IfStatement (conds elses) 
	    (letrec ((select-case
		      (lambda (lst)
			(if (pair? lst)
			    (let ((h (car lst)))
			      (let ((expr (car h)))
				(if (is-true (eval-expr env ctx expr))
				    (cdr h) 
				    (select-case (cdr lst)))
				))
			    elses))))
	      (fold (lambda (x ctx) (eval-statement env ctx x)) ctx (select-case conds))
	      ))

	 (ForStatement (iterator list-expr stmts) 
	     (let ((iterator
		    (cases texpr iterator
			   (IdentExpr (name) (list name))
			   (SetExpr (lst)  (ident-names-of lst))
			   (ListExpr (lst) (ident-names-of lst))
			   (else (error 'eval-statement "invalid iterator" iterator)))))
	       (iter ctx iterator
		     (lambda (ctx)
		       (fold (lambda (x ctx) (eval-statement env ctx x)) ctx stmts))
		     (eval-expr env ctx list-expr))
	       ))

	 (BlockStatement  (idexpr endexpr stmts)
	     (let* ((name    (ident-expr->name idexpr))
                    (endname (or (and endexpr (ident-expr->name endexpr)) name)))
               (if (equal? name endname)
                   (fold (lambda (x ctx) (eval-statement env ctx x)) ctx stmts)
                   (error 'eval-statement "mismatch between block begin name and end name"
                          name endname))))

	 
	 (CallStatement (idexpr call-args-def macro-args call-stmts)
	     (let* ((name (ident-expr->name idexpr))
                    (mac (get-macro ctx name)))
	       (if mac
		   (let ((call-arg-names (ident-names-of call-args-def))
			 (call-defaults (kwargs-of env ctx call-args-def)))
		     (let* ((ctx (set-macro ctx 'caller
					    (make-template-macro call-arg-names call-defaults call-stmts)))
			    (text (eval-expr env ctx (ApplyExpr (IdentExpr name) macro-args)))
			    (ctx  (pop-macro ctx)))
		       (cases tvalue text 
			      (Tnull () (begin))
			      (else (tvalue-output ctx (Tstr (->string text)))))
		       ctx
		       ))
                   (error 'eval-statement "macro not found" name)
                   )
               ))

	 (IncludeStatement (path w)
            (if w
		(let ((stmts (statements-from-file env path)))
		  (fold (lambda (x ctx) (eval-statement env ctx x)) ctx stmts))
		(let ((ctx1 (init-context env)))
		  (let ((stmts (statements-from-file env path)))
		    (fold (lambda (x ctx) (eval-statement env ctx x)) ctx1 stmts)
		    ctx))
		))
	 
	 (WithStatement (binds stmts) 
	    (let* ((kwargs (kwargs-of env ctx binds))
		   (names  (map car kwargs))
		   (values (map cdr kwargs))
		   (ctx    (push-frame ctx))
		   (ctx    (set-values ctx names values))
		   (ctx    (fold (lambda (s ctx) (eval-statement env ctx s)) ctx stmts)))
	      (pop-frame ctx)))

	 (AutoEscapeStatement (expr stmts) 
	     (let ((ctx (cases tvalue (eval-expr env ctx expr)
			       (Tbool (v)
				  (if v (set-filter ctx 'escape)
				      (set-filter ctx 'safe)))
			       (else
				(error 'eval-statement "invalid autoescape argument" expr)))))
	       (fold (lambda (s ctx) (eval-statement env ctx s)) ctx stmts)
	       (pop-filter ctx)
	       ))

	 (else  ctx)

	 ))


(define (safe-expr? x)
  (cases texpr x
	 (ApplyExpr (idexpr _)
		    (or
		     (eq? 'safe (ident-expr->name/safe idexpr))
		     (safe-expr? idexpr)))
	 (else #f)))



(define (unfold-extends env ctx stmts)
  (let recur ((ret '()) (stmts stmts))
    (if (null? stmts) ret
	(cases tstmt (car stmts)
	       (ExtendsStatement (path)
		    (let ((stmts1 (unfold-extends env ctx (statements-from-file env path))))
		      (recur (append ret stmts1) (cdr stmts))))
	       (else (recur (append ret (list (car stmts))) (cdr stmts)))
	       ))
    ))


(define (align-block stmts)

  (define (same-block? name stmt)
    (cases tstmt stmt
	   (BlockStatement (idexpr _ _)
	     (let ((name1 (ident-expr->name/safe idexpr)))
	       (equal? name1 name)))
	   (else #f)))

  (define (erase-block name lst)
    (filter (lambda (x) (not (same-block? name x))) lst))

  (let recur ((ret '()) (stmts stmts))
    (if (null? stmts) (reverse ret)
	(begin
	(let ((block (car stmts)))
	  (cases tstmt block
		 (BlockStatement (idexpr _ _)
		  (let* ((name (ident-expr->name idexpr))
			 (block1 (find (lambda (x) (same-block? name x)) (cdr stmts))))
		    (if block1
			(recur (cons block1 ret)
			       (erase-block name (cdr stmts)))
			(recur (cons block ret) (cdr stmts)))))
		 (else (recur (cons block ret) (cdr stmts))))
	  )))
    ))



(define (import-macro env ctx codes #!key  (namespace #f) (select #f))

  (let ((macro-name  (lambda (name) (if namespace (string->symbol (sprintf "~A.~A" namespace name)) name)))
	(alias-name  (lambda (name) (if select (alist-ref name select) name)))
	(can-import? (lambda (name) (if select (assoc name select) #t))))

    (fold (lambda (code ctx)
	    
	    (cases tstmt code
		   
		   (MacroStatement (idexpr def-args stmts)
		     (let ((name (ident-expr->name idexpr)))
		       (if (can-import? name)
			   (let ((arg-names (ident-names-of def-args))
				 (kwargs (kwargs-of env ctx def-args)))
			     (let ((full-name (macro-name (alias-name name))))
			       (set-macro ctx 
					  full-name
					  (make-template-macro arg-names kwargs stmts))))
			   ctx
			   )))
		   
		   (BlockStatement (x x1 stmts)
		      (import-macro env ctx stmts namespace: namespace select: select))

		   (IncludeStatement (path namespace)
		      (import-macro env ctx 
				    (statements-from-file env path)
				    namespace: namespace select: select ))
				     
		   (ImportStatement (path namespace)
		    (let ((res (import-macro env ctx (statements-from-file env path)
				  namespace: namespace
				  select: select)))
		      res))
		    

		   (FromImportStatement (path select-macros)
		      (let ((alias-names (alias-names-of select-macros)))
			(let ((res (import-macro env ctx 
						 (statements-from-file env path)
						 namespace: namespace
						 select: alias-names
						 )))
			  
			  res
			  )))
		   
		   (else ctx))
	    )
	  ctx codes)

    ))



(define (eval-statements codes #!key
			 (env (template-std-env))
			 (models '())
			 (ctx #f))

  (let* ((ctx (or ctx (init-context env: env models: models)))
	 (codes (align-block (unfold-extends env ctx codes)))
	 (ctx (import-macro env ctx codes)))

    (fold (lambda (s ctx) (eval-statement env ctx s)) ctx codes)

    (let ((output (get-output-string (tmpl-ctx-buffer ctx))))
      output)
    ))
