;; -*- Hen -*-
;;
;; The lexical analyzer for Ersatz.
;;
;; Based on the Jingoo library by Masaki WATANABE.
;;
;; Copyright 2012-2018 Ivan Raikov.
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

;;
;; This procedure generates a textual lexer description in the format
;; required by SILex.
;; 

(import scheme (chicken base) (chicken format) utf8-srfi-13)

(define (make-ersatz-lexer output-port
			   #!key 
			   (begin-comment "{#")
			   (end-comment "#}")
			   (begin-expand "{{")
			   (end-expand '("}}" "}}\n"))
			   (begin-logic "{%")
			   (end-logic '("%}" "%}\n"))
			   )

  (let ((begin-comment-len (string-length begin-comment))
	(end-comment-len (string-length end-comment)))

  (fprintf output-port #<<EOF
blank    [\9\32]
identfst [A-Za-z]
identchr [A-Za-z0-9_]
intlit   -?[0-9][0-9]*
floatlit -?[0-9]+(\.[0-9]*)?([eE][-+]?[0-9]+)?



%%


EOF
)


  (fprintf output-port #<<EOF
~S       (let ((start ~S) (end ~S))
                (cases lexer-mode (lexer-curmode)
                      (LexerPlain ()
			    (let loop ((kont yycontinue)
				       (depth 1))
			      (let ((c (yygetc)))
				(cond ((eq? 'eof c) 
				       (lexer-error "unexpected end of comment"))
				      ((lexer-lookahead c end ~A yygetc yyungetc)
				       (kont))
				      ((lexer-lookahead c start ~A yygetc yyungetc)
				       (loop (lambda () (loop kont depth)) (+ 1 depth)))
				      (else (loop kont depth))
				      ))
			      ))
			(else
			 (lexer-error "unexpected comment"))))

EOF
   begin-comment 
   begin-comment 
   end-comment
   begin-comment-len
   end-comment-len)


  (fprintf output-port #<<EOF
~A   (cases lexer-mode (lexer-curmode)
	    (LexerPlain ()
		(lexer-curmode (LexerExpand))
		(let ((text (lexer-get-text)))
		  (if (string-null? text)
		      (tok yyline EXPAND)
		      (begin
			(lexer-token-cache (cons (tok yyline EXPAND) (lexer-token-cache)))
			(tok yyline TEXT text)
			))
		  ))
	     (else 
	      (lexer-error "unexpected expansion"))
	     )

EOF
  (cond ((string? begin-expand) 
         (sprintf "~S" begin-expand))
        ((pair? begin-expand) 
         (string-join (map (lambda (s) (sprintf "~S" s)) begin-expand) "|"))
        (else (error 'ersatz "invalid begin-expand specification" begin-expand))))

  (fprintf output-port #<<EOF
~A   (cases lexer-mode (lexer-curmode)
	    (LexerExpand ()
		(lexer-curmode (LexerPlain))
		(tok yyline ENDEXPAND))
	     (else
	      (lexer-error "unexpected end of expansion"))
	     )

EOF
  (cond ((string? end-expand) 
         (sprintf "~S" end-expand))
        ((pair? end-expand) 
         (string-join (map (lambda (s) (sprintf "~S" s)) end-expand) "|"))
        (else (error 'ersatz "invalid end-expand specification" end-expand))))

  (fprintf output-port #<<EOF
~A     (cases lexer-mode (lexer-curmode)
	      (LexerPlain ()
		 (let ((text (lexer-get-text)))
		   (lexer-curmode (LexerLogic))
		   (if (string-null? text)
		       (yycontinue)
		       (tok yyline TEXT text))))
	     (else
	      (lexer-error "unexpected logic mode"))
	     )

EOF
  (cond ((string? begin-logic) 
         (sprintf "~S" begin-logic))
        ((pair? begin-logic) 
         (string-join (map (lambda (s) (sprintf "~S" s)) begin-logic) "|"))
        (else (error 'ersatz "invalid begin-logic specification" begin-logic))))

  (fprintf output-port #<<EOF
~A     (cases lexer-mode (lexer-curmode)
	      (LexerLogic ()
		(lexer-curmode (LexerPlain))
		(yycontinue))
	     (else (lexer-error "unexpected end of logic mode"))
	     )

EOF
  (cond ((string? end-logic) 
         (sprintf "~S" end-logic))
        ((pair? end-logic) 
         (string-join (map (lambda (s) (sprintf "~S" s)) end-logic) "|"))
        (else (error 'ersatz "invalid end-logic specification" end-logic))))


(fprintf output-port #<<EOF

"\\{" (begin
       (display #\{ (lexer-text-buffer))
       (yycontinue))


"\\}" (begin
       (display #\} (lexer-text-buffer))
       (yycontinue))


"\"" (cases lexer-mode (lexer-curmode)
	    (LexerPlain ()
	       (display #\" (lexer-text-buffer))
	       (yycontinue))
	    (else
	     (let loop ([cs '()])
	       (let ([c (yygetc)])
		 (cond [(eq? 'eof c)   (lexer-error "unexpected end of string constant")]
		       [(char=? c #\\) (let ((n (yygetc)))
					 (loop (cons n cs)))]
		       [(char=? c #\") (tok yyline STRING (reverse-list->string cs)) ]
		       [else (loop (cons c cs))])))
	     ))

"\'" (cases lexer-mode (lexer-curmode)
	    (LexerPlain ()
	       (display #\' (lexer-text-buffer))
	       (yycontinue))
	    (else
	     (let loop ([cs '()])
	       (let ([c (yygetc)])
		 (cond [(eq? 'eof c)   (lexer-error "unexpected end of string constant")]
		       [(char=? c #\\) (let ((n (yygetc)))
					 (loop (cons n cs)))]
		       [(char=? c #\') (tok yyline STRING (reverse-list->string cs)) ]
		       [else (loop (cons c cs))])))
	     ))

"\n"   (begin
	 (display #\newline (lexer-text-buffer))
	 (yycontinue))


{intlit} (cases lexer-mode (lexer-curmode)
		(LexerPlain ()
		    (begin
		      (display yytext (lexer-text-buffer))
		      (yycontinue)))
		(else
		 (tok yyline INT (string->number yytext)))
		)


{floatlit} (cases lexer-mode (lexer-curmode)
		  (LexerPlain ()
		      (begin
			(display yytext (lexer-text-buffer))
			(yycontinue)))
		  (else
		   (let ((n (string-length yytext)))
		     (tok yyline FLOAT (string->number
					(substring yytext 0 n)))))
		   )

{identfst}{identchr}*  (cases lexer-mode (lexer-curmode)
			      (LexerPlain ()
			        (begin
				  (display yytext (lexer-text-buffer))
				  (yycontinue)))
			      (else
			       (let*
				   ((word (string->symbol yytext))
				    (t (alist-ref word lexer-keywords)))
				 (if t (tok yyline ,t) 
				     (tok yyline IDENT word))))
			      )

"."{identfst}{identchr}*  (cases lexer-mode (lexer-curmode)
			      (LexerPlain ()
			        (begin
				  (display yytext (lexer-text-buffer))
				  (yycontinue)))
			      (else
			       (let*
				   ((n (string-length yytext))
                                    (word (substring yytext 1 n)))
                                 (tok yyline DOTFIELD word))))

"=="  (cases lexer-mode (lexer-curmode)
	       (LexerPlain ()
			   (display yytext (lexer-text-buffer))
			   (yycontinue))
	       (else
		(tok yyline EQEQ)))

"**"  (cases lexer-mode (lexer-curmode)
	       (LexerPlain ()
			   (display yytext (lexer-text-buffer))
			   (yycontinue))
	       (else
		(tok yyline POWER)))

"||"  (cases lexer-mode (lexer-curmode)
	       (LexerPlain ()
			   (display yytext (lexer-text-buffer))
			   (yycontinue))
	       (else
		(tok yyline OR)))

"&&"  (cases lexer-mode (lexer-curmode)
	       (LexerPlain ()
			   (display yytext (lexer-text-buffer))
			   (yycontinue))
	       (else
		(tok yyline AND)))

"!="  (cases lexer-mode (lexer-curmode)
	       (LexerPlain ()
			   (display yytext (lexer-text-buffer))
			   (yycontinue))
	       (else
		(tok yyline NEQ)))

.     (begin
        (cases lexer-mode (lexer-curmode)
	       (LexerPlain ()
			   (display yytext (lexer-text-buffer))
			   (yycontinue))
	       (else
		(let* ((word yytext)
		       (t (find-operator word lexer-operators )))
                  (case (car t)
		    ((full)     (tok yyline ,(cadr t)))
		    ((partial)  (let ((c (yygetc)))
				  (if (char=? c (string-ref (cadr t) 1))
				      (tok yyline ,(caddr t))
				      (begin (yyungetc)
					     (tok yyline IDENT (string->symbol word))
                                      ))
                                   ))
		    (else (if (char-set-contains? char-set:whitespace (string-ref word 0))
			      (yycontinue)
			      (tok yyline IDENT (string->symbol word))))
		    ))
		  ))
        )

<<EOF>> (cases lexer-mode (lexer-curmode)
	       (LexerPlain ()
		  (let ((text (lexer-get-text)))
		    (if (string-null? text)
			'*eoi*
			(tok yyline TEXT text)
			)))
	       (else (lexer-error "unexpected end of input (lexer)")))
EOF
)))



