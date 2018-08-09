
(import scheme (chicken base) (chicken file))

(include "make-ersatz-lexer.scm")
(let ((out (open-output-file "ersatz.l")))
  (make-ersatz-lexer out)
  (close-output-port out))

(import silex)
(lex-tables "ersatz.l" "default-ersatz-lexer-table" "ersatz.l.scm" 'counters 'line 'code)

