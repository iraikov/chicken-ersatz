;;
;;
;; The Ersatz template library.
;;
;; Based on the Ocaml Jingoo library by Masaki WATANABE, which is in
;; turn based on the Python Jinja2 library.
;;
;; Copyright 2012-2020 Ivan Raikov.
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

(module ersatz

	(
         debug

	 from-string from-file
	 statements-from-string statements-from-file
	 eval-expr eval-statement eval-statements
	 template-std-env init-context

	 keep-lexer-table lexer-trace

	 template-environment? make-template-environment make-lexer make-lexer*
	 tmpl-env-autoescape tmpl-env-search-path tmpl-env-filters tmpl-env-lexer
	 
	 template-context? make-template-context
	 tmpl-ctx-frame-stack tmpl-ctx-macro-table tmpl-ctx-namespace-table tmpl-ctx-filter-table tmpl-ctx-buffer
	 
	 template-context-frame? 

	 template-macro? make-template-macro 
	 tmpl-mac-args tmpl-mac-defaults tmpl-mac-code
	 macro-code?
	 
	 tvalue tvalue? Tnull Tint Tbool Tfloat Tstr Tobj Tlist Tset Tfun Tvector 
         sexpr->tvalue tvalue->sexpr tvalue->pystr
	 
	 tstmt tstmt? TextStatement ExpandStatement IfStatement ForStatement IncludeStatement
	 ExtendsStatement ImportStatement FromImportStatement SetStatement BlockStatement NamespaceStatement
	 MacroStatement FilterStatement CallStatement WithStatement AutoEscapeStatement
         
	 texpr texpr? IdentExpr LiteralExpr NotOpExpr NegativeOpExpr PlusOpExpr MinusOpExpr
	 TimesOpExpr PowerOpExpr DivOpExpr ModOpExpr AndOpExpr OrOpExpr NotEqOpExpr EqEqOpExpr  
	 LtOpExpr GtOpExpr  LtEqOpExpr GtEqOpExpr DotExpr BracketExpr ApplyExpr ListExpr SetExpr ObjExpr     
	 TestOpExpr KeywordExpr AliasExpr InOpExpr    

	 eq-eq list-same obj-same

	 op-default op-length op-reverse op-append op-cons op-first op-last op-slice
	 op-plus op-minus op-times op-power op-div op-mod
	 op-abs op-round op-range op-toint op-tofloat
	 op-or op-and
	 op-upper op-lower op-join op-substring
	 op-replace op-truncate
	 op-capitalize op-title op-escape-html op-urlize op-striptags op-trim op-pad
	 op-wordcount op-sort op-dictsort op-map
	 op-list op-sublist op-batch op-groupby
         op-vector op-ref op-update op-subvector
	 )

	(import scheme (chicken base)
                (only (chicken file) file-exists? delete-file )
                (only (chicken process) system)
                (only (chicken process-context) current-directory)
                (only (chicken pathname) make-pathname)
                (only (chicken string) reverse-list->string ->string conc string-intersperse string-split)
                (only (chicken pretty-print) pp)
                (only (chicken format) fprintf sprintf printf)
                (only (chicken sort) sort)
                (only srfi-1 every any drop fold find filter take last first concatenate)
		(only (chicken irregex) string->irregex sre->irregex irregex-search irregex-split irregex-replace/all irregex-match-num-submatches
		      irregex-match-start-index)
                datatype lalr lalr-driver uri-generic
                (only utf8 string-length substring)
		(only utf8-srfi-13 string-null? string-every string-upcase
		      string-downcase string-titlecase string-concatenate string-trim-both string-pad
		      string-ci< string<)
		(only utf8-srfi-14 char-set:lower-case char-set:upper-case char-set:whitespace 
		      char-set char-set->string char-set-contains?)
		(only silex lex-tables lexer-make-IS lexer-make-lexer )
                (only vector-lib vector-fold)
		)

(define (execute explist)
  (define (smooth lst)
    (let ((slst (map ->string lst)))
      (string-intersperse (cons (car slst) (cdr slst)) " ") ) )
  (for-each
   (lambda (cmd)
     (system cmd))
   (map smooth explist)))

(define-syntax run
  (syntax-rules ()
    ((_ exp ...)
     (execute (list `exp ...)))))

(define debug (make-parameter 0))

;;
;; template environment
;;
;;  autoescape    : if true or a procedure, template variables are auto escaped when output
;;  search-path   : search path list; if empty, search current directory only
;;  filters       : user-defined filters
;;  lexer         : lexical analyzer to be used (allowing for customizable syntax)

(define-record-type template-environment
  (make-template-environment  autoescape search-path filters lexer )
  template-environment?
  (autoescape          tmpl-env-autoescape)
  (search-path         tmpl-env-search-path)
  (filters             tmpl-env-filters)
  (lexer               tmpl-env-lexer)
  )

;;
;; template lexical analyzer
;;
;;  lexer-table   : the lexer table procedure (created by silex)
;;  token-cache   : the lexer token cache parameter
;;  lexer-reset   : the lexer reset procedure

(define-record-type template-lexer
  (make-template-lexer  lexer-table token-cache lexer-reset)
  template-lexer?
  (lexer-table         tmpl-lexer-table)
  (token-cache         tmpl-lexer-cache)
  (lexer-reset         tmpl-lexer-reset)
  )



;;
;; template context
;;

(define-record-type template-context
  (make-template-context frame-stack macro-table namespace-table filter-table buffer)
  template-context?
  (frame-stack   tmpl-ctx-frame-stack)
  (macro-table   tmpl-ctx-macro-table)
  (namespace-table   tmpl-ctx-namespace-table)
  (filter-table  tmpl-ctx-filter-table)
  (buffer        tmpl-ctx-buffer))


(define (template-context-frame? lst) 
  (every (lambda (x) (and (string? (car x)) (tvalue? (cdr x)))) lst))


(define-record-type template-macro
  (make-template-macro args defaults code) 
  template-macro?
  (args     tmpl-mac-args)
  (defaults tmpl-mac-defaults)
  (code     tmpl-mac-code))


(define (list-of pred)
  (lambda (lst) (every pred lst)))



(define-datatype tvalue tvalue?
  (Tnull)
  (Tint    (i integer?))
  (Tbool   (b boolean?))
  (Tfloat  (n number?)) 
  (Tstr    (s string?))
  (Tobj    (x tvalue-alist?))
  (Tlist   (x tvalue-list?))
  (Tset    (x tvalue-list?))
  (Tfun    (p procedure?))
  (Tvector (v vector?)))


(define (tvalue-stringp v)
  (cases tvalue v
	 (Tstr (s) (Tbool #t))
	 (else (Tbool #f))))
  

(define (tvalue-intp v)
  (cases tvalue v
	 (Tint (i) (Tbool #t))
	 (else (Tbool #f))))
  

(define (tvalue-floatp v)
  (cases tvalue v
	 (Tfloat (i) (Tbool #t))
	 (else (Tbool #f))))
  

(define (tvalue-listp v)
  (cases tvalue v
	 (Tlist (l) (Tbool #t))
	 (else (Tbool #f))))
  

(define (tvalue-setp v)
  (cases tvalue v
	 (Tset (l) (Tbool #t))
	 (else (Tbool #f))))
  

(define (tvalue-objp v)
  (cases tvalue v
	 (Tobj (l) (Tbool #t))
	 (else (Tbool #f))))
  

(define (tvalue-funp v)
  (cases tvalue v
	 (Tfun (f) (Tbool #t))
	 (else (Tbool #f))))
  

(define (tvalue-vectorp v)
  (cases tvalue v
	 (Tvector (v) (Tbool #t))
	 (else (Tbool #f))))
  

(define tvalue-alist?
  (list-of (lambda (x) (and (symbol? (car x)) (tvalue? (cdr x))))))


(define tvalue-list? (list-of tvalue?))

(define texpr-alist?
  (list-of (lambda (x) (and (symbol? (car x)) (texpr? (cdr x))))))




(define-record-printer (tvalue x out)
  (cases tvalue x
	 (Tnull ()   (fprintf out "<null>"))
	 (Tint   (i) (fprintf out "~A" i))
	 (Tbool  (b) (fprintf out "~A" (if b "true" "false")))
	 (Tfloat (n) (fprintf out "~A" n))
	 (Tstr   (s) (fprintf out "~A" s))
	 (Tobj   (x) (fprintf out "<obj>"))
	 (Tlist  (x) (fprintf out "<list>"))
	 (Tset   (x) (fprintf out "<set>"))
	 (Tfun   (x) (fprintf out "<function>"))
	 (Tvector   (x) (fprintf out "<vector>"))
	 ))


(define-record-printer (texpr x out)
  (cases texpr x
         (IdentExpr (s)   (fprintf out "Ident(~A)" s))
         (LiteralExpr (v) (fprintf out "Literal (~A)" v))
         (NotOpExpr       (e) (fprintf out "Not (~A)" e))
         (NegativeOpExpr  (e) (fprintf out "Neg (~A)" e))
         (PlusOpExpr      (e1 e2) (fprintf out "Plus (~A,~A)" e1 e2))
         (MinusOpExpr     (e1 e2) (fprintf out "Minus (~A,~A)" e1 e2))
         (TimesOpExpr     (e1 e2) (fprintf out "Times (~A,~A)" e1 e2))
         (PowerOpExpr     (e1 e2) (fprintf out "Power (~A,~A)" e1 e2))
         (DivOpExpr       (e1 e2) (fprintf out "Div (~A,~A)" e1 e2))
         (ModOpExpr       (e1 e2) (fprintf out "Mod (~A,~A)" e1 e2))
         (AndOpExpr       (e1 e2) (fprintf out "And (~A,~A)" e1 e2))
         (OrOpExpr        (e1 e2) (fprintf out "Or (~A,~A)" e1 e2))
         (NotEqOpExpr     (e1 e2) (fprintf out "Neq (~A,~A)" e1 e2))
         (EqEqOpExpr      (e1 e2) (fprintf out "Eq (~A,~A)" e1 e2))
         (LtOpExpr        (e1 e2) (fprintf out "Lt (~A,~A)" e1 e2))
         (GtOpExpr        (e1 e2) (fprintf out "PGt (~A,~A)" e1 e2))
         (LtEqOpExpr      (e1 e2) (fprintf out "LtEq (~A,~A)" e1 e2))
         (GtEqOpExpr      (e1 e2) (fprintf out "GtEq (~A,~A)" e1 e2))
         (DotExpr         (e1 e2) (fprintf out "Dot (~A,~A)" e1 e2))
         (BracketExpr     (e1 e2) (fprintf out "Brk (~A,~A)" e1 e2))
         (ApplyExpr       (e a) (fprintf out "Apply (~A,~A)" e a))
         (ListExpr        (xs) (fprintf out "List (~A)" xs))
         (SetExpr         (xs) (fprintf out "Set (~A)" xs))
         (ObjExpr         (xs) (fprintf out "Obj (~A)" xs))
         (TestOpExpr      (e1 e2) (fprintf out "Test (~A,~A)" e1 e2))
         (KeywordExpr     (e1 e2) (fprintf out "Keyword (~A,~A)" e1 e2))
         (AliasExpr       (e1 e2) (fprintf out "Alias (~A,~A)" e1 e2))
         (InOpExpr        (e1 e2) (fprintf out "In (~A,~A)" e1 e2))
         ))

  
(define (type-string-of-tvalue v)
  (cases tvalue v
	 (Tnull ()   "null")
	 (Tint   (i) "int")
	 (Tbool  (b) "bool")
	 (Tfloat (n) "float")
	 (Tstr   (s) "string")
	 (Tobj   (x) "obj")
	 (Tlist  (x) "list")
	 (Tset   (x) "set")
	 (Tfun   (x) "function")
	 (Tvector   (x) "vector")
	 ))

(define (sexpr->tvalue x)
  (cond
   ((boolean? x)    (Tbool x))
   ((integer? x)    (Tint x))
   ((number? x)     (Tfloat x))
   ((string? x)     (Tstr x))
   ((char? x)       (Tstr (->string x)))
   ((symbol? x)     (Tstr (->string x)))
   ((procedure? x)  (Tfun x))
   ((vector? x)     (Tvector (list->vector (map sexpr->tvalue (vector->list x)))))
   ((null? x)       (Tlist '()))
   ((pair? x)
    (cond
     ((and (pair? (car x)) (symbol? (car (car x))))
      (Tobj (map (lambda (x) (cons (car x) (sexpr->tvalue (cdr x)))) x)))
     (else (Tlist (map sexpr->tvalue x)))))
   ((tvalue? x)  x)
   (else (error 'sexpr->tvalue "cannot convert sexpr to tvalue" x))
   ))
        

(define (tvalue->sexpr x)
  (cases tvalue x 
         (Tnull ()     '(tnull))
         (Tint  (i)    i)
         (Tbool  (b)   b)
         (Tfloat (n)   n)
         (Tstr   (s)   s)
         (Tobj   (fs)  (map (lambda (x) (cons (car x) (tvalue->sexpr (cdr x)))) fs))
         (Tlist  (vs)  (map tvalue->sexpr vs))
         (Tset   (vs)  (map tvalue->sexpr vs))
         (Tfun   (p)   p)
         (Tvector (v)  v)
         ))


(define (tvalue->pystr x)
  (define (join-list lst sep)
    (string-concatenate (intersperse lst sep)))
  (cases tvalue x 
         (Tnull ()     "None")
         (Tint  (i)    (sprintf "~A" i))
         (Tbool  (b)   (if b "True" "False"))
         (Tfloat (n)   (sprintf "~A" n))
         (Tstr   (s)   (sprintf "'''~A'''" s))
         (Tlist  (vs)  (sprintf "[~A]" (join-list (map tvalue->pystr vs) ", ")))
         (Tobj   (fs)  (sprintf "{~A}" (join-list (map (lambda (x) (sprintf "'~A' : ~A" (car x) 
                                                                            (tvalue->pystr (cdr x)))) fs) ", ")))
         (Tset   (vs)  (sprintf "{~A}" (join-list (map tvalue->pystr vs) ", ")))
         (Tfun   (p)   p)
         (Tvector (v)  (sprintf "#[~A]" (join-list (map tvalue->pystr (vector->list v)) ", ")))
         ))



;; Template function arguments
;;
;;   Arguments of template functions are defined as "tvalue list".
;;   The filtered target is the LAST argument of filter function.
;;
;;   For example, consider the following expansion of "x" with filter
;;   function "foo" (with no keyword arguments) {{x|foo(10,20)}}
;;
;;   The filter function "foo" takes 3 arguments, and internally is
;;   evaluated like this:
;;
;;   (foo 10 20 x)

(define tfun-args? tvalue-list?) 
  
;;  Template function keyword arguments
;;  Keyword arguments of function are defined as (string * tvalue) list.

(define tfun-kwargs? tvalue-alist?) 


(define-datatype tstmt tstmt?
  (TextStatement   (s string?))

  (ExpandStatement (e texpr?))

  (IfStatement     (cb (list-of template-cond-clause?))
 	           (el template-ast?))

  (ForStatement    (e1 texpr?)
		   (e2 texpr?)
		   (a  template-ast?))

  (IncludeStatement (s string?) (wcontext boolean?))

  (ExtendsStatement (s string?))

  (ImportStatement  (s string?) 
		    (w (lambda (x) (or (not x) (symbol? x)))))

  (FromImportStatement (s string?)
		       (w (list-of texpr?)))

  (SetStatement (e1 texpr?)
		(e2 texpr?))

  (NamespaceStatement (s symbol?) (bs texpr-alist?))

  (BlockStatement  (e texpr?) (f (lambda (x) (or (not x) (texpr? x))))
		   (b template-ast?))

  (MacroStatement  (e texpr?)  
		   (a (list-of texpr?))
		   (b template-ast?))

  (FilterStatement (e texpr?) 
		   (b template-ast?))

  (CallStatement (e texpr?) 
		 (a1 (list-of texpr?))
		 (a2 (list-of texpr?) )
		 (b template-ast?))

  (WithStatement (es (list-of texpr?)) 
		 (b template-ast?))

  (AutoEscapeStatement (e texpr?) 
		       (b template-ast?))
  )


(define-record-printer (tstmt x out)
  (cases tstmt x
         (TextStatement (s)
                        (fprintf out "<TextStatement \"~A\">" s))
         (ExpandStatement (e)
                          (fprintf out "<ExpandStatement ~A>" e))
         (IfStatement     (cb el)
                          (fprintf out "<IfStatement ~A ~A>" cb el))
         (ForStatement    (e1 e2 a)
                          (fprintf out "<ForStatement ~A ~A ~A>" e1 e2 a))
         (IncludeStatement (s wcontext)
                           (fprintf out "<IncludeStatement ~A ~A>" s wcontext))
         (ExtendsStatement (s)
                           (fprintf out "<ExtendsStatement ~A>" s))
         (ImportStatement  (s w)
                           (fprintf out "<ImportStatement ~A ~A>" s w))
         (FromImportStatement (s w)
                              (fprintf out "<FromImportStatement ~A ~A>" s w))
         (SetStatement (e1 e2)
                       (fprintf out "<SetStatement ~A ~A>" e1 e2))
         (NamespaceStatement (s bs)
                             (fprintf out "<NamespaceStatement ~A : ~A>" s bs))
         (BlockStatement  (e f b)
                          (fprintf out "<BlockStatement ~A ~A ~A>" e f b))
         (MacroStatement  (e a b)
                          (fprintf out "<MacroStatement ~A ~A ~A>" e a b))
         (FilterStatement (e b)
                          (fprintf out "<FilterStatement ~A ~A>" e b))
         (CallStatement (e a1 a2 b)
                        (fprintf out "<CallStatement ~A ~A ~A ~A>" e a1 a2 b))
         (WithStatement (es b)
                        (fprintf out "<WithStatement ~A ~A>" es b))
         (AutoEscapeStatement (e b)
                              (fprintf out "<AutoEscapeStatement ~A ~A>" e b))
         ))




(define macro-code? (list-of tstmt?))

(define-datatype texpr texpr?

  (IdentExpr       (s symbol?))
  (LiteralExpr     (v tvalue?))
  (NotOpExpr       (e texpr?))
  (NegativeOpExpr  (e texpr?))
  (PlusOpExpr      (e1 texpr?) (e2 texpr?))
  (MinusOpExpr     (e1 texpr?) (e2 texpr?))
  (TimesOpExpr     (e1 texpr?) (e2 texpr?))
  (PowerOpExpr     (e1 texpr?) (e2 texpr?))
  (DivOpExpr       (e1 texpr?) (e2 texpr?))
  (ModOpExpr       (e1 texpr?) (e2 texpr?))
  (AndOpExpr       (e1 texpr?) (e2 texpr?))
  (OrOpExpr        (e1 texpr?) (e2 texpr?))
  (NotEqOpExpr     (e1 texpr?) (e2 texpr?))
  (EqEqOpExpr      (e1 texpr?) (e2 texpr?))
  (LtOpExpr        (e1 texpr?) (e2 texpr?))
  (GtOpExpr        (e1 texpr?) (e2 texpr?))
  (LtEqOpExpr      (e1 texpr?) (e2 texpr?))
  (GtEqOpExpr      (e1 texpr?) (e2 texpr?))
  (DotExpr         (e1 texpr?) (e2 texpr?))
  (BracketExpr     (e1 texpr?) (e2 texpr?))
  (ApplyExpr       (e texpr?) (a (list-of texpr?)))
  (ListExpr        (xs (list-of texpr?)))
  (SetExpr         (xs (list-of texpr?)))
  (ObjExpr         (xs (list-of expression-pair?)))
  (TestOpExpr      (e1 texpr?) (e2 texpr?))
  (KeywordExpr     (e1 texpr?) (e2 texpr?))
  (AliasExpr       (e1 texpr?) (e2 texpr?))
  (InOpExpr        (e1 texpr?) (e2 texpr?))
  )

(define (expression-pair? x) (and (texpr? (car x)) (texpr? (cdr x))))

(define template-ast? (list-of tstmt?))

(define (template-cond-clause? x) 
  (and (texpr? (car x)) (template-ast? (cdr x))))


(define (template-std-env #!key 
                          (autoescape #t) 
			  (search-path '()) 
			  (filters '()) 
			  (lexer (make-template-lexer default-ersatz-lexer-table lexer-token-cache reset-lexer))
			  )
  (make-template-environment
   autoescape search-path filters lexer)
  )


(include "runtime.scm")

;(define default-ersatz-lexer-table)

(define top-frame 
  `(
    ;; built-in filters 
    (abs        . ,(func-arg1 op-abs))
    (capitalize . ,(func-arg1 op-capitalize))
    (escape     . ,(func-arg1 op-escape-html))
    (e          . ,(func-arg1 op-escape-html)) ;; alias for escape 
    (float      . ,(func-arg1 op-tofloat))
    (int        . ,(func-arg1 op-toint))
    (first      . ,(func-arg1 op-first))
    (last       . ,(func-arg1 op-last))
    (length     . ,(func-arg1 op-length))
    (list       . ,(func-arg1 op-list))
    (lower      . ,(func-arg1 op-lower))
    (safe       . ,(func-arg1 op-safe))
    (strlen     . ,(func-arg1 op-strlen))
    (striptags  . ,(func-arg1 op-striptags))
    (sort       . ,(func-arg1 op-sort))
    (op-map     . ,(func-arg2 op-map))
    (upper      . ,(func-arg1 op-upper))
    (reverse    . ,(func-arg1 op-reverse))
    (append     . ,(func-arg2 op-append))
    (allCombs   . ,(func-arg2 op-allCombs))
    (cons       . ,(func-arg2 op-cons))
    (title      . ,(func-arg1 op-title))
    (trim       . ,(func-arg1 op-trim))
    (pad        . ,(func-arg2 op-pad))
    (urlize     . ,(func-arg1 op-urlize))
    (wordcount  . ,(func-arg1 op-wordcount))

    (dict    . ,(func-arg1 op-dict))
    (keys    . ,(func-arg1 op-keys))
    (attr    . ,(func-arg2 op-attr))
    (haskey  . ,(func-arg2 op-haskey))

    (groupby . ,(func-arg2 op-groupby))
    (batch   . ,(func-arg2 op-batch))
    (default . ,(func-arg2 op-default))
    (d       . ,(func-arg2 op-default)) ;; alias for default 
    (join    . ,(func-arg2 op-join))
    (split   . ,(func-arg2 op-split))
    (slice   . ,(func-arg2 op-slice))
    (truncate . ,(func-arg2 op-truncate))
    (range   . ,(func-arg2 op-range))
    (round   . ,(func-arg2 op-round))
    
    (replace   . ,(func-arg3 op-replace))
    (substring . ,(func-arg3 op-substring))
    (sublist   . ,(func-arg3 op-sublist))
    (batch     . ,(func-arg2 op-batch))

    ;; vector operations
    (vector       . ,(func-arg1 op-vector))
    (subvector    . ,(func-arg3 op-subvector))
    (ref          . ,(func-arg2 op-ref))
    (update       . ,(func-arg3 op-update))
    
    ;; built-in tests 
    (divisibleby . ,(func-arg2 test-divisibleby))
    (even        . ,(func-arg1 test-even))
    (iterable    . ,(func-arg1 test-iterable))
    (lower       . ,(func-arg1 test-lower))
    (number      . ,(func-arg1 test-number))
    (odd         . ,(func-arg1 test-odd))
    (sameas      . ,(func-arg2 test-sameas))
    (sequence    . ,(func-arg1 test-sequence))
    (string      . ,(func-arg1 test-string))
    (upper       . ,(func-arg1 test-upper))
    ))


(define (init-context #!key 
		      (env (template-std-env)) 
		      (models '())
		      (open-buffer open-output-string)
		      )
  (let ((env-values 
	 `((is_autoescape . ,(Tbool (and (tmpl-env-autoescape env) #t))))))
    (make-template-context 
     (list (append env-values models)
           (append (tmpl-env-filters env) 
                   top-frame)) ;; frame-stack 
     '() ;; macro-table
     '() ;; namespace-table
     '() ;; filter-table 
     (open-buffer) ;; buffer
     )))


(include "make-ersatz-lexer.scm")
(include "parser.scm")
(include "eval.scm")



(define (from-file fn #!key
		   (env (template-std-env))
		   (models '())
		   (ctx #f))
  (eval-statements (statements-from-file env fn)
		   env: env models: models ctx: ctx))


(define (from-string source #!key
		     (env (template-std-env))
		     (models '())
		     (ctx #f))
  (eval-statements (statements-from-string env source)
		   env: env models: models ctx: ctx))


)
