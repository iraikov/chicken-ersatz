
(import silex)

(include "make-ersatz-lexer.scm")

(make-ersatz-lexer (open-output-file "ersatz.l"))

(lex-tables "ersatz.l" "default-ersatz-lexer-table" "ersatz.l.scm" 'counters 'line 'code)

