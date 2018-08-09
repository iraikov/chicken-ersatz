;;
;;
;; Parsing routines for the Ersatz template library.
;;
;; Based on the Ocaml Jingoo library, which is in turn based on the
;; Python Jinja2 library.
;;
;; Copyright 2012-2018 Ivan Raikov
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


(define-syntax tok
  (syntax-rules ()
    ((tok ln t) (make-lexical-token (quasiquote t) ln #f))
    ((tok ln t l) (make-lexical-token (quasiquote t) ln l))
    ))

(define lexer-trace (make-parameter #f))

(define keep-lexer-table (make-parameter #f))

(define (make-lexer #!key 
                    (begin-comment "{#")
                    (end-comment "#}")
                    (begin-expand "{{")
                    (end-expand "}}")
                    (begin-logic "{%")
                    (end-logic "%}") 
                    (compile #t)
                    (lexer-proc (gensym 'ersatz-lexer-table)))
  
  (let* ((lexer-fn (->string lexer-proc))
	 (lexer-fn-scm (string-append (->string lexer-proc) ".scm"))
	 (output-port (open-output-file lexer-fn)))

    (make-ersatz-lexer output-port
		       begin-comment: begin-comment
		       end-comment: end-comment
		       begin-expand: begin-expand
		       end-expand: end-expand
		       begin-logic: begin-logic
		       end-logic: end-logic)

    (close-output-port output-port)

    (lex-tables lexer-fn lexer-fn lexer-fn-scm
		'counters 'line 'code)

    (let* ((in (open-input-file lexer-fn-scm)) 
	   (contents (read in)))

      (close-input-port in)

      (let ((module-contents 
	     `(module ,lexer-proc * (import scheme chicken) 

		      (import (only data-structures alist-ref))
		      (require-extension datatype lalr-driver)
		      (require-library utf8 utf8-srfi-13 utf8-srfi-14)
	
		      (import	(only utf8 string-length substring)
				(only utf8-srfi-13 string-null? string-every string-upcase
				      string-downcase string-titlecase string-concatenate string-join string-trim-both
				      string-ci< string<)
				(only utf8-srfi-14 char-set:lower-case char-set:upper-case char-set:whitespace 
				      char-set char-set->string char-set-contains?)
				)


		      (define-syntax tok
			(syntax-rules ()
			  ((tok ln t) (make-lexical-token (quasiquote t) ln #f))
			  ((tok ln t l) (make-lexical-token (quasiquote t) ln l))
			  ))


		      (define-syntax lexer-yyungetcn
			(syntax-rules ()
			  ((lexer-ungetcn i ungetc)
			   (let recur ((ii i))
			     (if (positive? ii)
				 (begin (ungetc) 
					(recur (- ii 1))))
			     ))
			  ))

		      
		      (define-syntax lexer-lookahead
			(syntax-rules ()
			  ((lexer-lookahead c str n getc ungetc)
			   (and (char=? (string-ref str 0) c)
				(let recur ((i 1))
				  (if (< i n) 
				      (if (char=? (string-ref str i) (getc))
					  (recur (+ 1 i)) 
					  (begin (lexer-yyungetcn i ungetc) #f))
				      #t))
				))
			  ))

		      (define-datatype lexer-mode lexer-mode?
			(LexerPlain)
			(LexerExpand)
			(LexerLogic))

		      (define lexer-curmode (make-parameter (LexerPlain)))

		      (define lexer-text-buffer (make-parameter (open-output-string)))
		      
		      (define lexer-string-buffer (make-parameter (open-output-string)))
		      
		      (define lexer-token-cache (make-parameter '()))
		      
                      (define (reset-lexer)
                        (lexer-text-buffer (open-output-string))
                        (lexer-string-buffer (open-output-string))
                        (lexer-curmode (LexerPlain))
                        (lexer-token-cache '()))

		      
		      (define-syntax lexer-get-string
			(syntax-rules ()
			  ((lexer-get-string) 
			   (let ((str (get-output-string (lexer-string-buffer))))
			     (close-output-port (lexer-string-buffer))
			     (lexer-string-buffer (open-output-string))
			     str))
			  ))
		      
		      
		      (define-syntax lexer-get-text
			(syntax-rules ()
			  ((lexer-get-text) 
			   (let ((text (get-output-string (lexer-text-buffer))))
			     (close-output-port (lexer-text-buffer))
			     (lexer-text-buffer (open-output-string))
			     text))
			  ))
		      
		      
		      (define lexer-keywords
			'((true . TRUE)
			  (false . FALSE)
			  (null . NULL)
			  (if . IF)
			  (else . ELSE)
			  (elseif . ELSEIF)
			  (elif . ELSEIF)
			  (endif . ENDIF)
			  (for . FOR)
			  (endfor . ENDFOR)
			  (include . INCLUDE)
			  (extends . EXTENDS)
			  (block . BLOCK)
			  (endblock . ENDBLOCK)
			  (filter . FILTER)
			  (endfilter . ENDFILTER)
			  (macro . MACRO)
			  (endmacro . ENDMACRO)
			  (call . CALL)
			  (endcall . ENDCALL)
			  (import . IMPORT)
			  (as . AS)
			  (from . FROM)
			  (in . IN)
			  (set . SET)
			  (not . NOT)
			  (and . AND)
			  (or . OR)
			  (is . IS)
			  (with . WITH)
			  (endwith . ENDWITH)
			  (without . WITHOUT)
			  (context . CONTEXT)
			  (autoescape . AUTOESCAPE)
			  (endautoescape . ENDAUTOESCAPE)
			  ))
		      

		      (define lexer-operators	
			'(("," .  COMMA)
			  ("=" .  EQ)
			  ("<" .  LT)
			  (">" .  GT)
			  ("!" .  NOT)
			  ("." .  DOT)
			  ("+" .  PLUS)
			  ("-" .  MINUS)
			  ("*" .  TIMES)
			  ("/" .  DIV)
			  ("%" .  MOD)
			  ("(" .  LPAREN)
			  (")" .  RPAREN)
			  ("[" .  LBRACKET)
			  ("]" .  RBRACKET)
			  ("{" .  LBRACE)
			  ("}" .  RBRACE)
			  (":" .  COLON)
			  ("|" .  VLINE)
			  ))


		      (define (find-operator word opers)
			(let ((wlen (string-length word))
			      (cmpfn (lambda (x y) (char=? (string-ref x 0) (string-ref y 0)))))
			  (let ((kv (assoc word opers cmpfn)))
			    (if kv
				(cond ((= wlen 1) `(full ,(cdr kv)))
				      ((> wlen 1) `(partial ,(car kv) ,(cdr kv)))
				      (else (error 'find-operator "invalid operator word" word)))
				'(none)
				))
			  ))
	      
		      
		      (define lexer-error error)

		      ,contents)))

	(let ((out (open-output-file lexer-fn-scm)))
	  (pp module-contents out)
	  (close-output-port out))))

    (if compile
	(begin
	  (run (csc -s ,lexer-fn-scm -j ,lexer-fn) 
               (csc -s ,(string-append lexer-fn ".import.scm")))
	  (load (string-append "./" lexer-fn ".so"))
	  (load (string-append "./" lexer-fn ".import.scm"))
	  )
        (load lexer-fn-scm))
    (let ((tbl (eval (string->symbol (conc lexer-proc "#" lexer-proc))))
          (ltc (eval (string->symbol (conc lexer-proc "#lexer-token-cache" ))))
          (rst (eval (string->symbol (conc lexer-proc "#reset-lexer" )))))
      (if (not (keep-lexer-table))
	  (for-each delete-file (list lexer-fn lexer-fn-scm )))
      (make-template-lexer tbl ltc rst)
    ))
  )


(define (make-lexer* lexer-proc)
  (let ((tbl (eval (string->symbol (conc lexer-proc "#" lexer-proc))))
        (ltc (eval (string->symbol (conc lexer-proc "#lexer-token-cache" ))))
        (rst (eval (string->symbol (conc lexer-proc "#reset-lexer" )))))
    (make-template-lexer tbl ltc rst)
    ))


(define-syntax lexer-yyungetcn
  (syntax-rules ()
    ((lexer-ungetcn i ungetc)
     (let recur ((ii i))
       (if (positive? ii)
	   (begin (ungetc) 
		  (recur (- ii 1))))
       ))
    ))

		      
(define-syntax lexer-lookahead
  (syntax-rules ()
    ((lexer-lookahead c str n getc ungetc)
       (and (char=? (string-ref str 0) c)
	    (let recur ((i 1))
	      (if (< i n) 
		  (let ((cc (getc)))
		    (if (char=? (string-ref str i) cc)
			(recur (+ 1 i)) 
			(begin (lexer-yyungetcn i ungetc) #f)))
		  #t))
	    ))
    ))

(define-datatype lexer-mode lexer-mode?
  (LexerPlain)
  (LexerExpand)
  (LexerLogic))


(define lexer-curmode (make-parameter (LexerPlain)))

(define lexer-text-buffer (make-parameter (open-output-string)))

(define lexer-string-buffer (make-parameter (open-output-string)))

(define lexer-token-cache (make-parameter '()))


(define-syntax lexer-get-string
  (syntax-rules ()
    ((lexer-get-string) 
     (let ((str (get-output-string (lexer-string-buffer))))
       (close-output-port (lexer-string-buffer))
       (lexer-string-buffer (open-output-string))
       str))
    ))


(define-syntax lexer-get-text
  (syntax-rules ()
    ((lexer-get-text) 
     (let ((text (get-output-string (lexer-text-buffer))))
       (close-output-port (lexer-text-buffer))
       (lexer-text-buffer (open-output-string))
       text))
    ))


(define lexer-keywords
  '((true . TRUE)
    (false . FALSE)
    (null . NULL)
    (if . IF)
    (else . ELSE)
    (elseif . ELSEIF)
    (elif . ELSEIF)
    (endif . ENDIF)
    (for . FOR)
    (endfor . ENDFOR)
    (include . INCLUDE)
    (extends . EXTENDS)
    (block . BLOCK)
    (endblock . ENDBLOCK)
    (filter . FILTER)
    (endfilter . ENDFILTER)
    (macro . MACRO)
    (endmacro . ENDMACRO)
    (call . CALL)
    (endcall . ENDCALL)
    (import . IMPORT)
    (as . AS)
    (from . FROM)
    (in . IN)
    (set . SET)
    (not . NOT)
    (and . AND)
    (or . OR)
    (is . IS)
    (with . WITH)
    (endwith . ENDWITH)
    (without . WITHOUT)
    (context . CONTEXT)
    (autoescape . AUTOESCAPE)
    (endautoescape . ENDAUTOESCAPE)
    ))

(define lexer-operators	
  '(("," .  COMMA)
    ("=" .  EQ)
    ("<" .  LT)
    (">" .  GT)
    ("!" .  NOT)
    ("." .  DOT)
    ("+" .  PLUS)
    ("-" .  MINUS)
    ("*" .  TIMES)
    ("/" .  DIV)
    ("%" .  MOD)
    ("(" .  LPAREN)
    (")" .  RPAREN)
    ("[" .  LBRACKET)
    ("]" .  RBRACKET)
    ("{" .  LBRACE)
    ("}" .  RBRACE)
    (":" .  COLON)
    ("|" .  VLINE)
    ))


(define (find-operator word opers)

  (let ((wlen (string-length word))
	(cmpfn (lambda (x y) (char=? (string-ref x 0) (string-ref y 0)))))
    (let ((v (alist-ref word opers cmpfn)))
      (if v
	  (cond ((= wlen 1) `(full ,v))
		((> wlen 1) `(partial ,word ,v))
		(else (error 'find-operator "invalid operator word" word)))
	  '(none)
	  ))
    ))
	      

(define lexer-error error)

(include "ersatz.l.scm")

(define (make-parse-error loc)
  (lambda (msg #!optional arg)
    (let ((loc-str (or (and loc (if (list? loc) (conc " " loc " ") (conc " (" loc ") "))) " ")))
      (cond  [(not arg) (error loc-str msg)]
	     [(lexical-token? arg)
	      (let ((src (lexical-token-source arg))
		    (cat (lexical-token-category arg))
                    (val (lexical-token-value arg)))
		(error (sprintf "~Aline ~A ~A " 
				loc-str
				(if (integer? src) src (source-location-line src)) 
				msg)
		       (or (and cat (sprintf "~A ~A" cat (and val ""))) "")
		       ))]
	     [else (error loc-str (conc msg arg))]
	     ))))


(include "ersatz.grm.scm")

(define (parse lexer-info is #!key (file-path #f))

    (let ((lexer (lexer-make-lexer (tmpl-lexer-table lexer-info) is))
          (lexer-token-cache (tmpl-lexer-cache lexer-info))
          (reset-lexer (tmpl-lexer-reset lexer-info)))

      (reset-lexer)

      (let ((ast
             (reverse
              (parser
               (if (not (lexer-trace))
                   
                   (lambda () 
                     (let ((c (lexer-token-cache)))

                       (let ((t (if (null? c) (lexer)
                                    (let ((t (car c)))
                                      (lexer-token-cache (cdr c))
                                      t))))
                         t)))
                   
                   (lambda () 
                     
                     (let ((c (lexer-token-cache)))
                       
                       (let ((t (if (null? c) (lexer)
                                    (let ((t (car c)))
                                      (lexer-token-cache (cdr c))
                                      t))))
                         
                         (if (lexical-token? t)
                             (let ((val (lexical-token-value t))
                                   (cat (lexical-token-category t)))
                               (sprintf "lexer: token = ~A~A" 
                                        cat (or (and val (sprintf " (~A)" val)) "")))
                             (print "lexer: token = " t))
                         
                         t))))
               
               (make-parse-error file-path)))))
	ast)))


(define (get-file-path fn #!key (template-dirs '()))
  (if (null? template-dirs)
      (let ((file-path (make-pathname (current-directory) fn)))
	(if (file-exists? file-path)
	    file-path
	    (error 'get-file-path "file not found" fn)))
      (let ((dir (car template-dirs)))
	(let ((file-path (make-pathname dir fn)))
	  (if (file-exists? file-path)
	      file-path
	      (get-file-path fn template-dirs: (cdr template-dirs)))))
      ))


(define (statements-from-file env fn)

  (let ((fpath (get-file-path fn template-dirs: (tmpl-env-search-path env))))

    (let* ((inport (if (file-exists? fpath)
		       (open-input-file fpath)
		       (error 'statements-from-file "input file not found " fpath)))
	   (is ;; lexer input system
	    (lexer-make-IS 'port inport)))

      (let ((stmts (parse (tmpl-env-lexer env) is file-path: fpath)))
	(close-input-port inport)
	stmts
      ))
    ))


(define (statements-from-string env source #!key (file-path #f))

  (let ((is ;; lexer input system
	 (if (string? source)
	     (lexer-make-IS 'string source)
	     (error 'statements-from-string "bad argument type; not a string" source))
	 ))

    (parse (tmpl-env-lexer env) is file-path: file-path)

    ))

(define (reset-lexer)
  (lexer-text-buffer (open-output-string))
  (lexer-string-buffer (open-output-string))
  (lexer-curmode (LexerPlain))
  (lexer-token-cache '()))


