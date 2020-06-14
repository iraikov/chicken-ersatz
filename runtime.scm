;;
;;
;; Runtime functions for the Ersatz template library.
;;
;; Based on the Ocaml Jingoo library, which is in turn based on the
;; Python Jinja2 library.
;;
;; Copyright 2012-2020 Ivan Raikov
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

; make-char-quotator QUOT-RULES
;
; Given QUOT-RULES, an assoc list of (char . string) pairs, return
; a quotation procedure. The returned quotation procedure takes a string
; and returns either a string or a list of strings. The quotation procedure
; check to see if its argument string contains any instance of a character
; that needs to be encoded (quoted). If the argument string is "clean",
; it is returned unchanged. Otherwise, the quotation procedure will
; return a list of string fragments. The input straing will be broken
; at the places where the special characters occur. The special character
; will be replaced by the corresponding encoding strings.
;
; For example, to make a procedure that quotes special HTML characters,
; do
;	(make-char-quotator
;	    '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;")))

(define (make-char-quotator char-encoding)
  (let ((bad-chars (map car char-encoding)))

    ; Check to see if str contains one of the characters in charset,
    ; from the position i onward. If so, return that character's index.
    ; otherwise, return #f
    (define (index-cset str i charset)
      (let loop ((i i))
	(and (< i (string-length str))
	     (if (memv (string-ref str i) charset) i
		 (loop (+ 1 i))))))

    ; The body of the function
    (lambda (str)
      (let ((bad-pos (index-cset str 0 bad-chars)))
	(if (not bad-pos) str	; str had all good chars
	    (let loop ((from 0) (to bad-pos))
	      (cond
	       ((>= from (string-length str)) '())
	       ((not to)
		(cons (substring str from (string-length str)) '()))
	       (else
		(let ((quoted-char
		       (cdr (assv (string-ref str to) char-encoding)))
		      (new-to 
		       (index-cset str (+ 1 to) bad-chars)))
		  (if (< from to)
		      (cons
		       (substring str from to)
		       (cons quoted-char (loop (+ 1 to) new-to)))
		      (cons quoted-char (loop (+ 1 to) new-to))))))))))
))

;; Returns the first i elements of list x; if the length of x is less
;; than i, pads the return result with fill-value
(define (take/pad x i fill-value)
  (let recur ((i i) (x x) (res '()))
    (if (positive? i)
	(if (null? x) 
	    (recur (- i 1) x (cons fill-value res))
	    (recur (- i 1) (cdr x) (cons (car x) res)))
	(reverse res))))
  

(define (unbox-int x)
  (cases tvalue x
	 (Tint (i) i)
	 (else (error 'unbox-int "invalid argument" x))))

(define (unbox-float x)
  (cases tvalue x
	 (Tfloat (f) f)
	 (else (error 'unbox-float "invalid argument" x))))

(define (unbox-string x)
  (cases tvalue x
	 (Tstr (s) s)
	 (else (error 'unbox-string "invalid argument" x))))

(define (unbox-bool x)
  (cases tvalue x
	 (Tbool (b) b)
	 (else (error 'unbox-bool "invalid argument" x))))

(define (unbox-list x)
  (cases tvalue x
	 (Tlist (lst) lst)
	 (else (error 'unbox-list "invalid argument" x))))

(define (unbox-set x)
  (cases tvalue x
	 (Tset (lst) lst)
	 (else (error 'unbox-set "invalid argument" x))))

(define (unbox-vector x)
  (cases tvalue x
	 (Tvector (v) v)
	 (else (error 'unbox-vector "invalid argument" x))))

(define (unbox-obj x)
  (cases tvalue x
	 (Tobj (alst) alst)
	 (else (error 'unbox-obj "invalid argument" x))))


(define (ident-or-string-expr->name expr)
  (cases texpr expr
	 (IdentExpr (name) name)
	 (LiteralExpr (v) 
                      (cases tvalue v 
                             (Tstr (s) (string->symbol s))
                             (else (error 'ident-or-string-expr->name 
                                          "invalid identifier expression"
                                          expr))))
	 (else (error 'ident-expr->name 
		      "invalid identifier expression"
		      expr))))
		    

(define (ident-expr->name expr)
  (cases texpr expr
	 (IdentExpr (name) name)
	 (else (error 'ident-expr->name 
		      "invalid identifier expression"
		      expr))))
		    

(define (ident-expr->name/safe expr)
  (cases texpr expr
	 (IdentExpr (name) name)
	 (else #f)))

(define (merge-defaults defaults kwargs)
  (map (lambda (nv)
	 (let ((name (car nv)))
	   (cond ((assoc name kwargs) =>
		  (lambda (value) (cons name value)))
		 (else
		  nv))))
       defaults))
	       
(define (union-defaults defaults kwargs)
  (let recur ((alst (merge-defaults defaults kwargs))
	      (kwargs kwargs))
    (if (null? kwargs) alst
	(let ((nv (car kwargs)))
	  (let ((name (car nv)))
	    (if (assoc name alst)
		(recur alst (cdr kwargs))
		(recur (cons nv alst) (cdr kwargs)))
	    )))
    ))


(define (template-context-with-buffer ctx buffer)
  (make-template-context
   (tmpl-ctx-frame-stack ctx)
   (tmpl-ctx-macro-table ctx)
   (tmpl-ctx-namespace-table ctx)
   (tmpl-ctx-filter-table ctx)
   buffer))


(define (push-frame ctx)
    (and (template-context? ctx) 
	 (let ((frame-stack (tmpl-ctx-frame-stack ctx)))
	   (make-template-context (cons '() frame-stack)
				  (tmpl-ctx-macro-table ctx)
				  (tmpl-ctx-namespace-table ctx)
				  (tmpl-ctx-filter-table ctx)
				  (tmpl-ctx-buffer ctx)))))

(define (pop-frame ctx)
    (and (template-context? ctx) 
	 (let ((frame-stack (tmpl-ctx-frame-stack ctx)))
	   (if (null? frame-stack) ctx
	       (let ((frame-stack1 (if (null? (cdr frame-stack))
				       frame-stack
				       (cdr frame-stack))))
		 (make-template-context frame-stack1
					(tmpl-ctx-macro-table ctx)
					(tmpl-ctx-namespace-table ctx)
					(tmpl-ctx-filter-table ctx)
					(tmpl-ctx-buffer ctx))))
	   )))




(define (set-value ctx name value)
  (if (and (template-context? ctx) (symbol? name) (tvalue? value))
   (let ((frame-stack (tmpl-ctx-frame-stack ctx)))
     (if (null? frame-stack)
	 (make-template-context (cons `((,name . ,value)) frame-stack)
				(tmpl-ctx-macro-table ctx)
				(tmpl-ctx-namespace-table ctx)
				(tmpl-ctx-filter-table ctx)
				(tmpl-ctx-buffer ctx))
	 (let ((frame (car frame-stack)))
	   (make-template-context (cons (cons `(,name . ,value) frame) (cdr frame-stack))
				  (tmpl-ctx-macro-table ctx)
				  (tmpl-ctx-namespace-table ctx)
				  (tmpl-ctx-filter-table ctx)
				  (tmpl-ctx-buffer ctx)))))
   (error 'set-value "invalid arguments" ctx name value)))


(define (set-values ctx names values)
  (and (template-context? ctx)
       (fold (lambda (name value ctx)
	       (and (symbol? name) (tvalue? value)
		    (set-value ctx name value)))
	     ctx names values)))


(define (bind-names ctx names values)
  (cond ((and (null? (cdr names)) (tvalue? values))
	 (set-value ctx (car names) values))
	((and (pair? (cdr names)) (tvalue-setp values))
	 (set-values ctx names (unbox-set values)))
	(else ctx)))


(define (get-value ctx name)
  (let recur ((frame-stack (tmpl-ctx-frame-stack ctx)))
    (if (null? frame-stack)
        (let ((v (alist-ref name (tmpl-ctx-namespace-table ctx))))
          (or (and v (Tobj v)) (Tnull)))
	(cond ((assoc name (car frame-stack)) => cdr)
	      (else (recur (cdr frame-stack))))
	)))


(define (set-namespace ctx ns bindings)
  (and (template-context? ctx) (symbol? ns) (tvalue-alist? bindings)
       (let ((namespace-table (tmpl-ctx-namespace-table ctx)))
	 (make-template-context (tmpl-ctx-frame-stack ctx)
				(tmpl-ctx-macro-table ctx)
                                (cons (cons ns bindings) namespace-table)
				(tmpl-ctx-filter-table ctx)
				(tmpl-ctx-buffer ctx)))))

(define (extend-namespace ctx ns name value)
  (and (template-context? ctx) (symbol? ns) (symbol? name) (tvalue? value)
       (let ((namespace-table (tmpl-ctx-namespace-table ctx)))
         (let ((namespace-bindings (alist-ref ns namespace-table)))
           (and namespace-bindings
                (make-template-context (tmpl-ctx-frame-stack ctx)
                                       (tmpl-ctx-macro-table ctx)
                                       (alist-update ns (alist-update name value namespace-bindings) namespace-table)
                                       (tmpl-ctx-filter-table ctx)
                                       (tmpl-ctx-buffer ctx)))))
       ))

(define-inline (get-func ctx name)
  (let ((value (get-value ctx name)))
    (cases tvalue value
	   (Tfun (f) value)
	   (else (error 'get-func "undefined function" name)))
    ))


(define (set-macro ctx name macro)
  (and (template-context? ctx) (symbol? name) (template-macro? macro)
       (let ((macro-table (tmpl-ctx-macro-table ctx)))
	 (make-template-context (tmpl-ctx-frame-stack ctx)
				(cons (cons name macro) macro-table)
				(tmpl-ctx-namespace-table ctx)
				(tmpl-ctx-filter-table ctx)
				(tmpl-ctx-buffer ctx)))))


(define (get-macro ctx name)
  (and (template-context? ctx)
       (cond ((assoc name (tmpl-ctx-macro-table ctx)) => cdr)
	     (else #f))
       ))


(define (pop-macro ctx)
  (and (template-context? ctx)
       (let ((macro-table (tmpl-ctx-macro-table ctx)))
	 (make-template-context (tmpl-ctx-frame-stack ctx)
				(if (null? macro-table) '() (cdr macro-table))
				(tmpl-ctx-namespace-table ctx)
				(tmpl-ctx-filter-table ctx)
				(tmpl-ctx-buffer ctx)))))


(define (set-filter ctx name)
  (and (template-context? ctx)
       (let ((filter-table (tmpl-ctx-filter-table ctx)))
	 (make-template-context (tmpl-ctx-frame-stack ctx)
				(tmpl-ctx-macro-table ctx)
                                (tmpl-ctx-namespace-table ctx)
				(cons name filter-table)
				(tmpl-ctx-buffer ctx)))))


(define (pop-filter ctx)
  (and (template-context? ctx)
       (let ((filter-table (tmpl-ctx-filter-table ctx)))
	 (make-template-context (tmpl-ctx-frame-stack ctx)
				(tmpl-ctx-macro-table ctx)
                                (tmpl-ctx-namespace-table ctx)
				(if (null? filter-table) '() (cdr filter-table))
				(tmpl-ctx-buffer ctx)))))


(define (tfun-apply f args #!key (name "<lambda>") (kwargs '()))
  (cases tvalue f
	 (Tfun (fn) (fn args kwargs))
	 (else (error 'tfun-apply "invalid function" name f))))


(define (tfilters-apply ctx text filters #!key (autoescape #t) (safe #f))
  (let ((safe.text 
	 (fold (lambda (name safe.text)
		 (let ((safe (car safe.text))
		       (text (cdr safe.text)))
		   (cond ((eq? name 'safe)
			  (cons #t text))
			 ((and (eq? name 'escape)
			       autoescape)
			  (cons safe text))
			 (else
			  (cons safe
				(tfun-apply (get-func ctx name) 
					    (list text)
					    name: name)
				))
			 )))
	       (cons safe text)
	       filters
	       )))
    
    (let ((safe (car safe.text))
	  (text (cdr safe.text)))
      (if (or safe (not autoescape)) text
	  (if (boolean? autoescape)
	      (op-escape-html (Tstr (->string text)) '())
	      (autoescape (Tstr (->string text)) '()))
      ))
    ))


(define (tvalue-output ctx v #!key (autoescape #t) (safe #f))
  (let ((tbl (tmpl-ctx-filter-table ctx)))
    (cases tvalue v
	   (Tnull () (begin))
	   (else
	    (if (and safe (null? tbl))
		(display v (tmpl-ctx-buffer ctx))
		(display (tfilters-apply ctx v tbl safe: safe autoescape: autoescape)
			 (tmpl-ctx-buffer ctx))
		)
            )
	   )))
		
	
(define (tobj-lookup ctx oname pname)
  (let ((v (get-value ctx oname)))
    (cases tvalue v
	   (Tobj (alst)
		 (cond ((assoc pname alst) => cdr)
		       (else  (Tnull))))
	   (else (Tnull)))
    ))
	
(define (tobjval-lookup v pname)
  (cases tvalue v
         (Tobj (alst)
               (cond ((assoc pname alst) => cdr)
                     (else  (Tnull))))
         (else (Tnull)))
  )


(define (iter-make-ctx ctx iterator item len i)
  (let* (
         (cycle (Tfun (lambda (args kwargs)
                        (let ((alen (length args)))
                          (list-ref args (modulo i alen))))))
         (ctx   (push-frame ctx))
         (ctx   (bind-names ctx iterator item))
         (ctx   (set-value ctx 'loop
                           (Tobj `((index0    . ,(Tint i))
                                   (index     . ,(Tint (+ i 1)))
                                   (revindex0 . ,(Tint (- len i 1)))
                                   (revindex  . ,(Tint (- len i)))
                                   (first     . ,(Tbool (= i 0)))
                                   (last      . ,(Tbool (= i (- len 1))))
                                   (length    . ,(Tint len))
                                   (cycle     . ,cycle)))))
         
         )
    ctx
    )
  )


(define (iter ctx iterator f iterable)

  (let* ((lst (cases tvalue iterable
		    (Tlist (lst) lst)
		    (Tset (lst) lst)
		    (Tobj (alst) (map (lambda (x) (Tset (list (Tstr (->string (car x))) (cdr x)))) alst))
                    (Tvector (v) (vector->list v))
		    (else (error 'iter "object not iterable" iterable))))
	 (len (length lst)))


    (let recur ((ctx ctx) (i 0) (lst lst))

      (if (null? lst) ctx
	  
	  (let ((item (car lst)))
	    (let* ((ctx (iter-make-ctx ctx iterator item len i))
		   (ctx  (f ctx))
		   (ctx  (pop-frame ctx)))
	      
	      (recur ctx (+ i 1) (cdr lst))
	      )))
      )))


	      
(define (eval-macro  env ctx macro-name args kwargs macro f  #!key (caller #f))
  (let ((arg-names (tmpl-mac-args macro))
	(defaults  (tmpl-mac-defaults macro))
	(code      (tmpl-mac-code macro)))

    (let ((args-len      (length args))
	  (arg-names-len (length arg-names)))

      (if (< args-len arg-names-len)
          (error 'eval-macro "macro called with wrong number of arguments" 
                 (list macro-name arg-names)
                 args))

      (let* ((ctx  (push-frame ctx))
	     (ctx  (set-value ctx 'varargs (Tlist (drop args arg-names-len))))
	     (ctx  (set-value ctx 'kwargs  (Tobj kwargs)))
	     (ctx  (set-value ctx macro-name
			      (Tobj `((name         . ,(Tstr (->string macro-name)))
				      (arguments    . ,(Tlist (map (compose Tstr ->string) arg-names)))
				      (defaults     . ,(Tobj defaults))
				      (catch_kwargs . ,(Tbool (not (null? kwargs))))
				      (catch_vargs  . ,(Tbool (> args-len arg-names-len)))
				      (caller       . ,(Tbool caller))
				      ))
			      ))
	     (ctx (fold (lambda (name value ctx)
			  (set-value ctx name value))
			ctx arg-names (take/pad args arg-names-len (Tnull))))
	     (ctx (fold (lambda (name.value ctx)
			  (let ((name (car name.value))
				(value (cdr name.value)))
			  (set-value ctx name (or (alist-ref name kwargs) value))))
			ctx (merge-defaults defaults kwargs)))
	     (ctx (fold (lambda (name.value ctx)
			  (let ((name (car name.value))
				(value (cdr name.value)))
			    (set-value ctx name (or (alist-ref name kwargs) value))))
			ctx defaults))
	     (ctx (f ctx code)))
	(pop-frame ctx))
      ))
  )


(define (get-kvalue name kwargs #!key (defaults '()))
  (or (alist-ref name kwargs) (alist-ref name defaults) (Tnull)))


(define (op-safe value kwargs) value)


(define (op-attr prop obj kwargs)
  (cases tvalue obj
	 (Tobj (alst)
	       (cases tvalue prop
		      (Tstr (prop) 
                            (or (alist-ref (string->symbol prop) alst) (Tnull)))
		      (else (error 'attr "property type error" obj prop))))
	 (else (error 'attr "object type error" obj prop))))

(define (op-haskey obj prop kwargs)
  (cases tvalue obj
	 (Tobj (alst)
	       (cases tvalue prop
		      (Tstr (prop) 
                            (if (alist-ref (string->symbol prop) alst)
                                (Tbool #t)
                                (Tbool #f)))
		      (else (error 'haskey "property type error" obj prop))))
	 (else (error 'haskey "object type error" obj prop))))


(define (is-true x)
  (cases tvalue x
	 (Tbool (x)  x)
	 (Tstr  (x)  (not (string-null? x)))
	 (Tint  (x)  (not (= x 0)))
	 (Tfloat (x) (not (= x 0.0)))
	 (Tlist (x)  (> (length x) 0))
	 (Tset  (x)  (> (length x) 0))
	 (Tvector (x)  (> (vector-length x) 0))
	 (Tobj  (x)  (> (length x) 0))
	 (Tnull ()   #f)
	 (Tfun (f)   (error 'is-true "operand type error" x))))


(define (op-default value default kwargs)
  (cases tvalue value
	 (Tnull () default)
	 (else value)))


(define (op-negate x)
  (cases tvalue x
	 (Tint (x)   (Tint (- x)))
	 (Tfloat (x) (Tfloat (- x)))
	 (else (error 'negate "operand type error" x))))



(define (test-not x) (Tbool (not (is-true x))))


(define (test-none ctx name)
  (cases tvalue (get-value ctx name)
	 (Tnull () (Tbool #t))
	 (else (Tbool #f))))


(define (test-defined ctx name)
  (cases tvalue (get-value ctx name)
	 (Tnull () (Tbool #f))
	 (else (Tbool #t))))


(define (test-undefined ctx name)
  (cases tvalue (test-defined ctx name)
	 (Tbool (status) (Tbool (not status)))
	 (else (error 'test-undefined "invalid operand" name))))


(define (test-obj-defined ctx objname propname)
  (cases tvalue (get-value ctx objname)
	 (Tobj (alst) 
	       (Tbool (and (assoc propname alst) #t)))
	 (else (Tbool #f))))


(define (test-obj-undefined ctx objname propname)
  (cases tvalue (test-obj-defined ctx objname propname)
	 (Tbool (status) (Tbool (not status)))
	 (else (error 'test-obj-undefined "invalid operand" objname))))


(define (test-escaped ctx)
  (Tbool (member "safe" (tmpl-ctx-filter-table ctx))))


(define (test-divisibleby num target kwargs)
  (let ((n (unbox-int num))
	(t (unbox-int target)))
    (if (zero? n) 
	(Tbool #f) 
	(Tbool (zero? (modulo t n))))))
	 

(define (test-even num kwargs)
  (Tbool (zero? (modulo (unbox-int num) 2))))
	 

(define (test-odd num kwargs)
  (Tbool (= 1 (modulo (unbox-int num) 2))))

	 
(define (test-iterable x kwargs)
  (cases tvalue x
	 (Tlist (_) (Tbool #t))
	 (Tset (_)  (Tbool #t))
	 (Tvector (_)  (Tbool #t))
	 (Tobj (_)  (Tbool #t))
	 (else (Tbool #f))))

	 
(define (test-lower x kwargs)
  (cases tvalue x
	 (Tstr (str) (Tbool (string-every char-set:lower-case str)))
	 (else (Tbool #f))))

	 
(define (test-upper x kwargs)
  (cases tvalue x
	 (Tstr (str) (Tbool (string-every char-set:upper-case str)))
	 (else (Tbool #f))))

	 
(define (test-number x kwargs)
  (cases tvalue x
	 (Tint (i) (Tbool #t))
	 (Tfloat (f) (Tbool #t))
	 (else (Tbool #f))))

	 
(define (test-string x kwargs)
  (cases tvalue x
	 (Tstr (s) (Tbool #t))
	 (else (Tbool #f))))


(define (test-sameas value target kwargs)
  (cases tvalue value
	 (Tstr (x)
	       (cases tvalue target
		      (Tstr (y) (Tbool (equal? x y)))
		      (else (Tbool #f))))
	 (Tint (x)
	       (cases tvalue target
		      (Tint (y) (Tbool (equal? x y)))
		      (else (Tbool #f))))
	 (Tfloat (x)
	       (cases tvalue target
		      (Tfloat (y) (Tbool (equal? x y)))
		      (else (Tbool #f))))
	 (Tbool (x)
	       (cases tvalue target
		      (Tbool (y) (Tbool (equal? x y)))
		      (else (Tbool #f))))
	 (Tfun (x)
	       (cases tvalue target
		      (Tfun (y) (Tbool (equal? x y)))
		      (else (Tbool #f))))
	 (Tobj (x)
	       (cases tvalue target
		      (Tobj (y) (Tbool (equal? x y)))
		      (else (Tbool #f))))
	 (Tlist (x)
	       (cases tvalue target
		      (Tlist (y) (Tbool (equal? x y)))
		      (else (Tbool #f))))
	 (Tset (x)
	       (cases tvalue target
		      (Tset (y) (Tbool (equal? x y)))
		      (else (Tbool #f))))
	 (Tvector (x)
	       (cases tvalue target
		      (Tvector (y) (Tbool (equal? x y)))
		      (else (Tbool #f))))
	 (else (Tbool #f))
	 ))

	 
(define (test-sequence target kwargs)
  (test-iterable target kwargs))


(define (op-plus left right)
  (cases tvalue left
	 (Tint (x1)
	       (cases tvalue right
		      (Tint (x2)   (Tint (+ x1 x2)))
		      (Tfloat (x2) (Tfloat (+ x1 x2)))
		      (Tstr (x2)   (Tstr (sprintf "~A~A" x1 x2)))
		      (else (error 'plus "operand type error" left right))))
	 (Tfloat (x1)
	       (cases tvalue right
		      (Tint (x2)   (Tfloat (+ x1 x2)))
		      (Tfloat (x2) (Tfloat (+ x1 x2)))
		      (Tstr (x2)   (Tstr (sprintf "~A~A" x1 x2)))
		      (else (error 'plus "operand type error" left right))))
	 (Tstr (x1)
	       (cases tvalue right
		      (Tstr (x2)   (Tstr (string-append x1 x2)))
		      (Tint (x2)   (Tstr (sprintf "~A~A" x1 x2)))
		      (Tfloat (x2)   (Tstr (sprintf "~A~A" x1 x2)))
		      (else (error 'plus "operand type error" left right))))
	 
	 (else (error 'plus "operand type error" left right))
	 ))


(define (op-minus left right)
  (cases tvalue left
	 (Tint (x1)
	       (cases tvalue right
		      (Tint (x2)   (Tint (- x1 x2)))
		      (Tfloat (x2) (Tfloat (- x1 x2)))
		      (else (error 'minus "operand type error" left right))))
	 (Tfloat (x1)
	       (cases tvalue right
		      (Tint (x2)   (Tfloat (- x1 x2)))
		      (Tfloat (x2) (Tfloat (- x1 x2)))
		      (else (error 'minus "operand type error" left right))))
	 
	 (else (error 'minus "operand type error" left right))
	 ))


(define (op-times left right)
  (cases tvalue left
	 (Tint (x1)
	       (cases tvalue right
		      (Tint (x2)   (Tint (* x1 x2)))
		      (Tfloat (x2) (Tfloat (* x1 x2)))
		      (else (error 'times "operand type error" left right))))
	 (Tfloat (x1)
	       (cases tvalue right
		      (Tint (x2)   (Tfloat (* x1 x2)))
		      (Tfloat (x2) (Tfloat (* x1 x2)))
		      (else (error 'times "operand type error" left right))))
	 
	 (else (error 'times "operand type error" left right))
	 ))


(define (op-power left right)
  (letrec ((power1 (lambda (m n a)
		     (if (<= n 0) a
			 (if (zero? (modulo n 2))
			     (power1 (* m m) (/ n 2) a)
			     (power1 m (- n 1) (* m a)))))))
  (cases tvalue left
	 (Tint (x1)
	       (cases tvalue right
		      (Tint (x2)
			    (Tfloat (power1 x1 x2 1.0)))
		      (else (error 'power "operand type error" left right))))
	 (else (error 'power "operand type error" left right)))
  ))


(define (op-div left right)
  (cases tvalue right
	 (Tint (x2)
	       (if (zero? x2)
		   (error 'div "division by zero")
		   (cases tvalue left
			  (Tint (x1)
				(Tint (quotient x1 x2)))
			  (Tfloat (x1)
				  (Tfloat (/ x1 x2)))
			  (else
			   (error 'div "operand type error" left right)))
		   ))
	 (Tfloat (x2)
		 (if (zero? x2)
		     (error 'div "division by zero")
		     (cases tvalue left
			    (Tint (x1)
				  (Tfloat (/ x1 x2)))
			    (Tfloat (x1)
				    (Tfloat (/ x1 x2)))
			    (else
			     (error 'div "operand type error" left right)))
		     ))
	 (else
	  (error 'div "operand type error" left right))
	 ))


(define (op-mod left right)
  (cases tvalue right
	 (Tint (x2)
	       (if (zero? x2)
		   (error 'mod "division by zero")
		   (cases tvalue left
			  (Tint (x1) (Tint (modulo x1 x2)))
			  (else
			   (error 'mod "operand type error" left right)))
		   ))
	 (else
	  (error 'mod "operand type error" left right))
	 ))

(define (op-round how value kwargs)
  (cases tvalue value
	 (Tint (i) value)
	 (Tfloat (x)
		 (let ((how (string->symbol (unbox-string how))))
		   (case how
		     ((floor)  (Tfloat (floor x)))
		     ((ceil)   (Tfloat (ceiling x)))
		     (else (error 'round "unknown rounding method" how)))))
	 (else (error 'round "operand type error" value))))
		   

(define (op-abs value kwargs)
  (cases tvalue value
	 (Tint (x) (Tint (abs x)))
	 (else (error 'abs "operand type error" value))))


(define (op-and left right)
  (Tbool (and (is-true left) (is-true right))))


(define (op-or left right)
  (Tbool (or (is-true left) (is-true right))))


(define (eq-eq left right)
  (cases tvalue left
	 (Tint (x1)
	       (cases tvalue right
		      (Tint (x2) (Tbool (= x1 x2)))
		      (else (Tbool #f))))
	 (Tfloat (x1)
		 (cases tvalue right
			(Tfloat (x2) (Tbool (= x1 x2)))
			(else (Tbool #f))))
	 (Tbool (x1)
		 (cases tvalue right
			(Tbool (x2) (Tbool (equal? x1 x2)))
			(else (Tbool #f))))
	 (Tstr (x1)
		 (cases tvalue right
			(Tstr (x2) 
			      (Tbool (string=? x1 x2)))
			(else (Tbool #f))))
	 (Tlist (x1)
		 (cases tvalue right
			(Tlist (x2) (list-same left right))
			(else (Tbool #f))))
	 (Tvector (x1)
		 (cases tvalue right
			(Tvector (x2) (vector-same left right))
			(else (Tbool #f))))
	 (Tobj (x1)
		 (cases tvalue right
			(Tobj (x2) (obj-same left right))
			(else (Tbool #f))))
	 (else (Tbool #f))
	 ))


(define (list-same lst1 lst2)
  (let ((l1 (unbox-list lst1))
	(l2 (unbox-list lst2)))
    (if (not (= (length l1) (length l2))) 
	(Tbool #f)
	(let ((result (every (compose unbox-bool eq-eq) l1 l2)))
	  (Tbool result)))
    ))

(define (vector-same v1 v2)
  (let ((vec1 (unbox-vector v1))
	(vec2 (unbox-vector v2)))
    (if (not (= (vector-length vec1) (vector-length vec2))) 
	(Tbool #f)
	(let ((result (every (compose unbox-bool eq-eq) (vector->list vec1) (vector->list vec2))))
	  (Tbool result)))
    ))


(define (obj-same obj1 obj2)
  (let ((al1 (unbox-obj obj1))
	(al2 (unbox-obj obj2)))
    (if (not (= (length al1) (length al2))) 
	(Tbool #f)
	(let ((result (every (lambda (x) 
			       (let ((v (alist-ref (car x) al2)))
				 (and v (unbox-bool (eq-eq v (cdr x)))))) al1)))
	  (Tbool result)))
    ))


(define (not-eq left right)
  (cases tvalue left
	 (Tint (x1)
	       (cases tvalue right
		      (Tint (x2) (Tbool (not (= x1 x2))))
		      (else (Tbool #t))))
	 (Tfloat (x1)
	       (cases tvalue right
		      (Tfloat (x2) (Tbool (not (= x1 x2))))
		      (else (Tbool #t))))
	 (Tstr (x1)
	       (cases tvalue right
		      (Tstr (x2) (Tbool (not (string=? x1 x2))))
		      (else (Tbool #t))))
	 (else (Tbool #t))))


(define (op-lt left right)
  (cases tvalue left
	 (Tint (x1)
	       (cases tvalue right
		      (Tint (x2) (Tbool (< x1 x2)))
		      (else (error 'lt "operand type error" left right))))
	 (Tfloat (x1)
	       (cases tvalue right
		      (Tfloat (x2) (Tbool (< x1 x2)))
		      (else (error 'lt "operand type error" left right))))
	 (else (error 'lt "operand type error" left right))))


(define (op-gt left right)
  (cases tvalue left
	 (Tint (x1)
	       (cases tvalue right
		      (Tint (x2) (Tbool (> x1 x2)))
		      (else (error 'gt "operand type error" left right))))
	 (Tfloat (x1)
	       (cases tvalue right
		      (Tfloat (x2) (Tbool (> x1 x2)))
		      (else (error 'gt "operand type error" left right))))
	 (else (error 'gt "operand type error" left right))))


(define (op-lteq left right)
  (cases tvalue left
	 (Tint (x1)
	       (cases tvalue right
		      (Tint (x2) (Tbool (<= x1 x2)))
		      (else (error 'lteq "operand type error" left right))))
	 (Tfloat (x1)
	       (cases tvalue right
		      (Tfloat (x2) (Tbool (<= x1 x2)))
		      (else (error 'lteq "operand type error" left right))))
	 (else (error 'lteq "operand type error" left right))))


(define (op-gteq left right)
  (cases tvalue left
	 (Tint (x1)
	       (cases tvalue right
		      (Tint (x2) (Tbool (>= x1 x2)))
		      (else (error 'gteq "operand type error" left right))))
	 (Tfloat (x1)
	       (cases tvalue right
		      (Tfloat (x2) (Tbool (>= x1 x2)))
		      (else (error 'gteq "operand type error" left right))))
	 (else (error 'gteq "operand type error" left right))))


(define (op-in left right)
  (cases tvalue right
	 (Tlist (lst)
		(Tbool (any (compose unbox-bool (lambda (x) (eq-eq x left))) lst)))
	 (Tobj (alst)
		(Tbool (any (compose unbox-bool (lambda (x) (eq-eq (Tstr (->string (car x))) left))) alst)))
	 (else (Tbool #f))))



(define (op-upper x kwargs)
  (cases tvalue x
	 (Tstr (str) (Tstr (string-upcase str)))
	 (else  (Tstr (sprintf "~A" x)))))


(define (op-lower x kwargs)
  (cases tvalue x
	 (Tstr (str) (Tstr (string-downcase str)))
	 (else  (Tstr (sprintf "~A" x)))))


(define (op-capitalize value kwargs)
  (Tstr (string-titlecase (unbox-string value))))


(define (op-toint x kwargs)
  (cases tvalue x
	 (Tint (v)  (Tint (inexact->exact (round v))))
	 (Tfloat (v)  (Tint (inexact->exact (round v))))
	 (else (error 'toint "operand type error" x))))


(define (op-tofloat x kwargs)
  (cases tvalue x
	 (Tfloat (_)  x)
	 (Tint (v)  (Tfloat (exact->inexact v)))
	 (else (error 'tofloat "operand type error" x))))


(define (op-join join-str lst kwargs)
  (let ((str (unbox-string join-str)))
    (cases tvalue lst
	   (Tlist (lst)
		  (Tstr (string-concatenate (intersperse (map ->string lst) str))))
	   (Tvector (v)
		  (Tstr (string-concatenate (intersperse (map ->string (vector->list lst)) str))))
	   (Tset (lst)
		 (Tstr (string-concatenate (intersperse (map ->string lst) str))))
	   (else (error 'join "operand type error" join-str lst)))))


(define (op-split pat text kwargs)
  (let ((pat (unbox-string pat))
	(text (unbox-string text)))
    (let ((lst (irregex-split (string->irregex pat) text)))
      (Tlist (map Tstr lst)))))


(define (op-substring base count str kwargs)
  (let ((base  (unbox-int base))
	(count (unbox-int count))
	(str   (cases tvalue str
		      (Tstr (str) str)
		      (Tnull ()  "")
		      (else (error 'substring "operand type error" str)))))
    (if (string-null? str) 
	(Tstr "")
	(Tstr (substring str base (+ base count))))
    ))
	

(define (op-truncate len str kwargs)
  (let (
	(len (unbox-int len))
	(str (unbox-string str))
	)
    (if (string-null? str) 
	(Tstr "")
	(Tstr (substring str 0 len)))
    ))
    

(define (op-replace src dst str kwargs)
  (let (
	(src (unbox-string src))
	(dst (unbox-string dst))
	(str (unbox-string str))
	)
    (Tstr (irregex-replace/all (string->irregex src) str dst))
    ))


(define (op-trim str kwargs)
  (let ((str (unbox-string str)))
    (Tstr (string-trim-both str char-set:whitespace))))

(define (op-pad len str kwargs)
  (let ((len (unbox-int len))
        (str (unbox-string str)))
    (Tstr (string-pad str (+ (string-length str) len)))))


; procedure: string->goodHTML STRING
; Given a string, check to make sure it does not contain characters
; such as '<' or '&' that require encoding. Return either the original
; string, or a list of string fragments with special characters
; replaced by appropriate character entities.

(define string->goodHTML
  (make-char-quotator
   '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;"))))


(define (op-escape-html str kwargs)
  (let ((str (unbox-string str)))
    (let ((res (string->goodHTML str)))
      (Tstr (if (string? res) res (string-concatenate res))))))

(define url-pat
  (string->irregex "((http|ftp|https):\\/\\/[\\w\\-_]+(\\.[\\w\\-_]+)+([\\w\\-\\.,@?^=%&amp;:/~\\+#]*[\\w\\-\\@?^=%&amp;/~\\+#])?)"))

(define (op-urlize text kwargs)
  (let ((str (unbox-string text)))
    (Tstr (irregex-replace/all url-pat str
			       "<a href='" 1 "'>" 1 "</a>"))))


(define (op-title text kwargs)
  (let ((str (unbox-string text)))
    (Tstr (string-titlecase str))))


(define (op-striptags text kwargs)
  (let* ((str (unbox-string text))
	 (pat (string->irregex "<\\/?[^>]+>")))
    (Tstr (irregex-replace/all pat str ""))))

(define whitespace-pat (sre->irregex '($ (+ whitespace))))

(define (op-wordcount text kwargs)
  (let ((str (unbox-string text)))
    (Tint (length (irregex-split whitespace-pat str)))))


(define (op-strlen x kwargs)
  (cases tvalue x
	 (Tstr (str) (Tint (string-length str)))
	 (else (error 'strlen "operand type error" x))))


(define (op-length x kwargs)
  (cases tvalue x
	 (Tlist (lst) (Tint (length lst)))
	 (Tset (lst) (Tint (length lst)))
	 (Tvector (v) (Tint (vector-length v)))
	 (Tstr (str) (Tint (string-length str)))
	 (else (error 'length "operand type error" x))))


(define (op-reverse lst kwargs)
  (let ((lst (unbox-list lst)))
    (Tlist (reverse lst))))

(define (op-append x y kwargs)
  (let ((xlst (unbox-list x))
        (ylst (unbox-list y)))
    (Tlist (append xlst ylst))))

(define (allCombs . lsts)
  (if (null? lsts)
      '(())
      (let ((l (car lsts))
            (ls (cdr lsts)))
        (let ((cs (apply allCombs ls)))
          (concatenate (map (lambda (x) (map (lambda (c) (cons x c)) cs)) l))
          ))
      ))

(define (op-allCombs x y kwargs)
  (let ((xlst (unbox-list x))
        (ylst (unbox-list y)))
    (Tset (map Tset (allCombs xlst ylst)))))

(define (op-cons x y kwargs)
  (let ((ylst (unbox-list y)))
    (Tlist (cons x ylst))))


(define (op-last lst kwargs)
  (let ((lst (unbox-list lst)))
    (last lst)))

(define (op-first lst kwargs)
  (let ((lst (unbox-list lst)))
    (first lst)))

(define (op-ref vec i kwargs)
  (let ((vec (unbox-vector vec)))
    (vector-ref vec i)))

(define (op-update vec i val kwargs)
  (let ((vec (unbox-vector vec)))
    (vector-set! vec i val)))


(define (op-list value kwargs)
  (cases tvalue value
	 (Tlist (lst) value)
	 (Tset (lst)  (Tlist lst))
	 (Tstr (str)
	       (let ((len (string-length str)))
		 (let iter ((ret '()) (i len))
		   (if (zero? i)
		       (Tlist ret)
		       (let ((s1 (Tstr (substring str (- i 1) i))))
			 (iter (cons s1 ret) (- i 1)))
		       ))
		 ))
	 (Tvector (vec) (Tlist (vector->list vec)))
	 (else (error 'list "operand type error" value))
	 ))


(define (op-set value kwargs)
  (cases tvalue value
	 (Tlist (lst) value)
	 (Tset (lst)  (Tlist lst))
	 (Tstr (str)
	       (let ((len (string-length str)))
		 (let iter ((ret '()) (i len))
		   (if (zero? i)
		       (Tlist ret)
		       (let ((s1 (Tstr (substring str (- i 1) i))))
			 (iter (cons s1 ret) (- i 1)))
		       ))
		 ))
	 (else (error 'list "operand type error" value))
	 ))

(define (op-vector value kwargs)
  (cases tvalue value
	 (Tvector (vec) value)
	 (Tlist (lst) (Tvector (list->vector lst)))
	 (Tset (lst)  (Tvector (list->vector lst)))
	 (Tstr (str)
	       (let ((len (string-length str)))
		 (let iter ((ret '()) (i len))
		   (if (zero? i)
		       (Tvector (list->vector ret))
		       (let ((s1 (Tstr (substring str (- i 1) i))))
			 (iter (cons s1 ret) (- i 1)))
		       ))
		 ))
	 (else (error 'vector "operand type error" value))
	 ))


(define (op-slice len value kwargs #!key (defaults `((fill_with . ,(Tnull)))))
  (op-batch len (op-list value '()) kwargs))


(define (op-sublist base count lst kwargs)
  (let ((base (unbox-int base))
	(lst  (unbox-list lst)))
    (cases tvalue count
	   (Tint (count)
		 (Tlist (take (drop lst base) count)))
	   (Tnull ()
		  (Tlist (drop lst base)))
	   (else (error 'sublist "operand type error" count)))
    ))


(define (op-subvector base count v kwargs)
  (let ((base (unbox-int base))
	(vec  (unbox-vector v)))
    (cases tvalue count
	   (Tint (count)
		 (Tvector (subvector vec base count)))
	   (Tnull ()
		  (Tvector (subvector vec base)))
	   (else (error 'subvector "operand type error" count)))
    ))


(define (op-batch base lst kwargs)
  (let ((base (unbox-int base))
	(lst  (unbox-list lst)))
    (let recur ((lst lst) (groups '()))
      (if (< (length lst) base)
          (Tlist (reverse (cons (Tlist lst) groups)))
          (recur (drop lst base) 
                 (cons (Tlist (take lst base)) groups))
          ))
    ))


(define (op-range start stop kwargs)
  (let ((start (unbox-int start))
	(stop (unbox-int stop)))
    (if (= start stop)
	(Tlist (list (Tint start)))
	(let ((is-end? (lambda (i) (if (< start stop) (> i stop) (< i stop))))
	      (next (lambda (i) (if (< start stop) (+ i 1) (- i 1)))))
	  (let iter ((ret '()) (i start))
	    (if (is-end? i) 
		(Tlist (reverse ret))
		(iter (cons (Tint i) ret) (next i))))
	  ))
    ))


(define (op-batch count value kwargs #!key (defaults `((fill_with . ,(Tnull)))))
  (let ((slice-count (unbox-int count))
	(lst (unbox-list value)))
    (let ((fill-value 
	   (let ((v (get-kvalue 'fill_with kwargs defaults)))
	     (cases tvalue v
		    (Tnull () #f)
		    (else v)))))
      (let batch ((ret '()) 
		  (left-count (length lst))
		  (rest lst))
	(cond ((> left-count slice-count)
	       (batch (cons (Tlist (take rest slice-count)) ret)
			 (- left-count slice-count)
			 (drop rest slice-count)))
	      ((> left-count 0)
	       (batch (cons (Tlist (filter identity (take/pad rest slice-count fill-value))) ret) 0 '()))
	      (else (Tlist (reverse ret)))
	      ))
      ))
  )


(define (op-sort lst kwargs)
  (let ((lst (unbox-list lst)))
    (cases tvalue (car lst)
	   (Tstr (s)
		 (Tlist (map Tstr (sort	(cons s (map unbox-string (cdr lst))) string<))
			))
	   (Tint (i)
		 (Tlist (map Tint (sort (cons i (map unbox-int (cdr lst))) <))))
	   (Tfloat (i)
		   (Tlist (map Tfloat (sort (cons i (map unbox-float (cdr lst))) <))))
	   (else "operand type error" lst))
	   ))
		 

(define (op-dictsort val kwargs 
		     #!key (defaults  `((case_sensitive . ,(Tbool #t))
					(by             . ,(Tstr "key")))))

  (let ((cs (cases tvalue (get-kvalue 'case_sensitive kwargs defaults)
		   (Tbool (v) v)
		   (else (error 'dictsort "operand type error")))))

    (cases tvalue val
	   (Tobj (alst)
		 (Tobj (if cs  
			   (sort alst 
				 (lambda (a b) (string< (symbol->string (car a))
							(symbol->string (car b)))))
			   (sort alst 
				 (lambda (a b) (string-ci< (symbol->string (car a))
							   (symbol->string (car b)))))
				 )))
	   (else val))
))


(define (op-dict val kwargs) 
  (let ((alst (unbox-obj val)))
    (Tlist (map (lambda (key.val)
                  (let ((name (car key.val))
                        (val (cdr key.val)))
                    (Tobj `((name . ,(Tstr (->string name)))
                            (value . ,val)))))
                alst))
    ))
                        

(define (op-keys val kwargs) 
  (let ((alst (unbox-obj val)))
    (Tlist (map (lambda (key.val)
                  (let ((name (car key.val)))
                    (Tstr (->string name))))
                alst))
    ))
                        

(define (func-arg0 f) 
  (Tfun (lambda (args kwargs) 
	  (f kwargs))))

(define (func-arg1 f) 
  (Tfun (lambda (args kwargs) 
	  (if (= 1 (length args))
	      (f (car args) kwargs)
	      (Tnull)))
	))


(define (func-arg2 f) 
  (Tfun (lambda (args kwargs) 
	  (let ((len (length args)))
	    (cond ((= len 2) 
		   (f (car args) (cadr args) kwargs))
		  ((= len 1) 
		   (let ((arg1 (car args)))
		     (func-arg1 (lambda (arg2 kwargs) (f arg1 arg2 kwargs)))
		     ))
		  (else  (Tnull)))
	    ))
	))

(define (func-arg3 f) 
  (Tfun (lambda (args kwargs) 
	  (let ((len (length args)))
	    (cond ((= len 3) (f (car args) (cadr args) (caddr args) kwargs))
		  ((= len 2) 
		   (let ((arg1 (car args)) (arg2 (cadr args)))
		     (func-arg1 (lambda (arg3 kwargs) (f arg1 arg2 arg3 kwargs)))
		     ))
		  ((= len 1) 
		   (let ((arg1 (car args)))
		     (func-arg2 (lambda (arg2 kwargs) (f arg1 arg2 kwargs)))))
		  (else  (Tnull)))
	    ))
	))

