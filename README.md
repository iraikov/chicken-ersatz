# ersatz

A template engine compatible with Jinja2. 

## Documentation

The `ersatz` library provides a template engine somewhat compatible
with the Jinja2 Python template engine
(http://jinja.pocoo.org/docs/). It is a based on a port of the Ocaml
Jingoo library by Masaki WATANABE
(https://github.com/tategakibunko/jingoo).

### Procedures

#### Evaluation

<procedure>from-string :: STRING [env: TEMPLATE-ENVIRONMENT] [models: ALIST] [ctx: TEMPLATE-CONTEXT] -> STRING</procedure>

Parses the given string and evaluates the resulting template
statements in the given context and models. See the section on data
types for detailed definitions of the environment, context and
statement types.

<procedure>from-file :: STRING [env: TEMPLATE-ENVIRONMENT] [models: ALIST] [ctx: TEMPLATE-CONTEXT] -> STRING</procedure>

Parses the given file and evaluates the resulting template statements
in the given context and models. See the the next section and the data
types section for detailed definitions of the environment, context and
statement types.

<procedure>eval-expr :: TEMPLATE-ENVIRONMENT * TEMPLATE-CONTEXT * TEXPR -> TVALUE </procedure>

<procedure>eval-statement :: TEMPLATE-ENVIRONMENT * TEMPLATE-CONTEXT * TSTMT -> TEMPLATE-CONTEXT </procedure>

#### Template environment and context initalization

<procedure> template-std-env :: [autoescape: BOOL] [search-path: STRING LIST] [filters: STRING LIST] [lexer-table: LEXER-TABLE] -> TEMPLATE-ENVIRONMENT</procedure>

Creates a template evaluation environment to be used with
`init-context` and the parsing and evaluation procedures in the next
sections. The following optional arguments are recognized:

- `autoescape: BOOL` : indicates whether to escape special HTML characters in expansion statements (`#f` by default)
- `search-path: STRING LIST` : a list of directories to be searched for include files
- `filters: STRING LIST` : a list of filters to be applied to the inpurt
- `lexer-table: LEXER-TABLE` : a table generated by `make-lexer-table`

<procedure> init-context :: [env: TEMPLATE-ENVIRONMENT] [models: ALIST] [open-buffer: VOID -> PORT] -> TEMPLATE-CONTEXT</procedure>

Initializes template context. The following optional arguments are recognized:

- `env: TEMPLATE-ENVIRONMENT` : a structure created by `template-std-env` above
- `models: ALIST` : an association list of the form `(NAME . VALUE)` where `name` is a symbol, and `value` is a template value, described below; names in the `models` list are to be substituted in expansion statements found in the input
- `open-buffer: VOID -> PORT` : alternative procedure for creating the output port

#### Parsing

<procedure>statements-from-string :: TEMPLATE-ENVIRONMENT * STRING -> TSTMT LIST</procedure>

Parses the given string and returns a list of template statements. See
the section on data types for detailed definitions of the environment
and statement types.

<procedure>statements-from-file :: TEMPLATE-ENVIRONMENT * STRING -> TSTMT LIST</procedure>

Parses the given file and returns a list of template statements. See
the section on data types for detailed definitions of the environment
and statement types.

#### Conversion to and from template values

<procedure>sexpr->tvalue :: SEXPR -> TVALUE</procedure>

Converts an s-expression to its template value representation. See the
`tvalue` datatype, described below for details.

<procedure>tvalue->sexpr :: TVALUE -> SEXPR</procedure>

Converts a template value to its s-expression representation. See the
`tvalue` datatype, described below for details.

### Data types

#### Template values

`(define-datatype tvalue tvalue? ...)`

Representation of values in the template language. The value definitions are:

- `(Tnull)` : null value
- `(Tint   (i integer?))` : integers
- `(Tbool  (b boolean?))` : booleans
- `(Tfloat (n number?))` : floating point numbers
- `(Tstr   (s string?))` : strings
- `(Tobj   (x tvalue-alist?))` : template objects; represented as alists where the key is a symbol and the value is a `tvalue`
- `(Tlist  (x tvalue-list?))` : lists
- `(Tset   (x tvalue-list?))` : sets
- `(Tfun   (p procedure?))` : template functions

Template values are represented by the following s-expressions:

- `(Tnull)` : `'(tnull)`
- `(Tint   (i integer?))` : integer
- `(Tbool  (b boolean?))` : boolean
- `(Tfloat (n number?))` : number
- `(Tstr   (s string?))` : string
- `(Tobj   (x tvalue-alist?))` : alist
- `(Tlist  (x tvalue-list?))` : list
- `(Tset   (x tvalue-list?))` : vector
- `(Tfun   (p procedure?))` : procedure

#### Template environments

```
 (define-record-type template-environment
   (make-template-environment  autoescape search-path filters lexer-table )
   template-environment?
   (autoescape          tmpl-env-autoescape)
   (search-path         tmpl-env-search-path)
   (filters             tmpl-env-filters)
   (lexer-table         tmpl-env-lexer-table)
   )
```

- `autoescape`    : if true or a procedure, template variables are auto escaped when output (true by default)
- `search-path`   : search path list for including templates; if empty, search current directory only
- `filters`       : user-defined filters
- `lexer-table`   : lexical analyzer table to be used (allowing for customizable syntax of template directives)

#### Template contexts

```
 (define-record-type template-context
   (make-template-context frame-stack macro-table filter-table buffer)
   template-context?
   (frame-stack   tmpl-ctx-frame-stack)
   (macro-table   tmpl-ctx-macro-table)
   (filter-table  tmpl-ctx-filter-table)
   (buffer        tmpl-ctx-buffer))
```

- `framestack` : a stack where each element is a list of bindings of names to template values
- `macro-table` : the current table of macros
- `filter-table` : the current table of active filters
- `buffer` : the output string buffer

#### Template statements

- `(TextStatement   (s string?))` : Literal text
- `(ExpandStatement (e texpr?))` : substitutes the variables in the given expression with the respective values found in the current template context
- `(IfStatement     (cb (list-of template-cond-clause?)) (else template-ast?))` : evaluates the conditions in the list of clauses, and if any condition evaluates to true, executes its statement- otherwise executes the else statement
- `(ForStatement    (e1 texpr?) (e2 texpr?) (a template-ast?))` : exectues the statements in `a` for the given range
- `(IncludeStatement (s string?) (wcontext boolean?))` : includes the template located in the file specified by `s`- the flag `wcontext` specifies whether the current context will apply to the statements in the included template
- `(ExtendsStatement (s string?))` : 
- `(ImportStatement  (s string?) (w (lambda (x) (or (not x) (symbol? x)))))` : 
- `(FromImportStatement (s string?) (w (list-of texpr?)))` :
- `(SetStatement (e1 texpr?) (e2 texpr?))` : 
- `(BlockStatement  (e texpr?) (f (lambda (x) (or (not x) (texpr? x)))) (b template-ast?))` :
- `(MacroStatement  (e texpr?) (a (list-of texpr?)) (b template-ast?))` :
- `(FilterStatement (e texpr?) (b template-ast?))` :
- `(CallStatement (e texpr?) (a1 (list-of texpr?)) (a2 (list-of texpr?)) (b template-ast?))` :
- `(WithStatement (es (list-of texpr?)) (b template-ast?))` :
- `(AutoEscapeStatement (e texpr?) (b template-ast?))` :


## Examples

### `from-string`

```
  ;; expand and escape
  (from-string "{{danger}}" models: `((danger . ,(Tstr "<script>alert(1)</script>" ))))
   
  ==>
 
  "&lt;script&gt;alert(1)&lt;/script&gt;"
 
 
  ;; safe expand
  (from-string "{{danger|safe}}" models: `((danger . ,(Tstr "<script>alert(1)</script>" ))))
   
  ==>
 
  "<script>alert(1)</script>"

 ;; expand with filter
 (from-string "{{pi|int}}" models: (list (cons 'pi (Tfloat 3.14))))

 ==>

 "3"

 ;; if statement
 (let* ( (source "{% if x <= 1 %}one{% elseif x == 2 %}two{% else %}three{% endif %}") )
   (from-string source  models: `((x . ,(Tint 3)))))
  
  ==>

  "three"

 (print
   (let ( (source #<<EOF
 {% for p in posts %}
  <div class="blog-post">
    <h3>{{ p.title }}</h3>
    <div class="post-body">
     {{ p.body }}
    </div>
  </div>
 {% endfor %}
 EOF
 ))
  (from-string source
    models:
      `((posts . ,(Tlist
                   (list (Tobj `((title . ,(Tstr "Post One title"))
                                 (body  . ,(Tstr "Post One body"))
                          ))
                         (Tobj `((title . ,(Tstr "Post Two title"))
                                 (body  . ,(Tstr "Post Two body"))
                          ))
                   ))
                )
            )) 
 ))

 ==>

  <div class="blog-post">
    <h3>Post One title</h3>
    <div class="post-body">
     Post One body
    </div>
  </div>

  <div class="blog-post">
    <h3>Post Two title</h3>
    <div class="post-body">
     Post Two body
    </div>
  </div>
```

### `from-file`

```
  (from-file "cheatsheet.tmpl" env: (template-std-env search-path: '("dir/tmpl")))
```

### Template cheat sheet

> 
> include test
> ============
> 
> {% include "header.tmpl" %}
> 
> binop
> ======
> 
> 1+1 = {{ 1 + 1 }}
> 1-1 = {{ 1 - 1 }}
> 2*3 = {{ 2 * 3 }}
> 4/2 = {{ 4 / 2 }}
> 8^3 = {{ 8 ** 3 }}
> 8%3 = {{ 8 % 3 }}
> not true = {{ !true }}
> not true(2) = {{ not true }} 
> 
> single quote string test
> =========================
> 
> {% set single_quoted = 'hoge' %}
> single_quoted = {{single_quoted}}
> 
> expand test
> ===========
> 
> {% set danger = "<script>alert(1)</script>" %}
> expand with escape = {{ danger }}
> expand with safe = {{ danger|safe }}
> 
> set test
> =========
> 
> set hoge = "ok"
> {% set hoge = "ok" %}
> now hoge = {{ hoge }}
> 
> if test
> =======
> 
> {% if hoge == "ok" %}
> value of hoge == "ok"
> {% else %}
> value of hoge != "ok"
> {% endif %}
> 
> for test
> ========
> 
> 
> {% for item in [1,2,3,4,5] %}
> {% set tmp = "hoge" %}
> <p>{{item}}</p>
> <p>{{loop.cycle(1,2,3)}}</p>
> <p>tmp = {{item}}</p>
> {% endfor %}
> 
> {% for href, title in [ ("http://yahoo.co.jp", "yahoo japan") ] %}
> <a href="{{href}}">{{title}}</a>
> {% endfor %}
> 
> {% for href, title in [("http://yahoo.co.jp", "yahoo japan"), ("http://google.co.jp", "google japan")] %}
> <a href="{{href}}">{{title}}</a>
> {% endfor %}
> 
> {% for href, title in [("http://yahoo.co.jp", "yahoo japan"), ("http://google.co.jp", "google japan")] %}
> <a href="{{href}}">{{title}}</a>
> {% endfor %}
> 
>  obj test
> =========
> 
> {% set obj = { age:10, name: 'aa' } %}
> 
> name = {{obj.name}}
> age = {{obj.age}}
> obj["name"] = {{ obj["name"] }}
> obj["age"] = {{ obj["age"] }}
> 
> filter test
> ===========
> 
> upper test:{{ "must be upper"|upper }}
> word count for "hoge hage hige" = {{ "hoge hage hige"|wordcount}}
> 
> func test
> =========
> 
> range(0,3) = {% for x in range(0,3) %}{{x}}{% endfor %}
> range(3,0) = {% for x in range(3,0) %}{{x}}{% endfor %}
> 
> strlen("testtest") = {{ strlen("testtest") }}
> strlen("日本語") = {{ strlen("日本語") }}
> 
> round floor of 1.5 = {{1.5|round("floor")|int}}
> round ceil of 1.5 = {{1.5|round("ceil")|int}}
> 
> join(",", [1,2,3,4,5]) = {{ join(",", [1,2,3,4,5]) }}
> 
> {% with long_list =  [10,20,30,40,50,60,70,80,90,100] %}
> {% for row in slice(4, long_list) %}
>   {% set y = loop.index %}
>   {% for col in row %}
>   {% set x = loop.index %}
>   {{x}},{{y}} = {{col}}
>   {% endfor %}
> {% endfor %}
> {% endwith %}
> 
> 
> filter tag test
> ===============
> 
> {% filter upper %}
> must be upper
> {% endfilter %}
> 
> list expr
> =========
> 
> {% for x in [1,2,3,4.5,"str"] %}{{x}}{% endfor %}
> {% for x in [] %}{{x}}{% endfor %}
> 
> {{ join("-", [1,2,3]) }}
> 
> {{ "{{" }}
> 
> syntax test "is"
> ================
> 
> 6 is divisibleby 4 = {{ 6 is divisibleby(4) }}
> 6 is divisibleby 3 = {{ 6 is divisibleby(3) }}
> 6 is divisibleby 2 = {{ 6 is divisibleby(2) }}
> 6 is divisibleby 2 = {{ 6 is divisibleby 2 }}
> 6 is divisibleby 3 via func = {{ divisibleby(3,6) }}
> 
> macro test
> ==========
> 
> {% macro hoge_macro(i,j) %}
> {{i}},{{j}}
> {% endmacro %}
> 
> {{ hoge_macro(10,20) }}
> 
> {# at this point, delay_macro is not declared, but we can call it. #}
> {{ delay_macro(10,20) }}
> 
> {% macro delay_macro(x,y) %}
> {{x}} {{ caller(1,2) }} {{y}}
> {% endmacro %}
> 
> {% call(a,b) delay_macro("from", "to") %}
> inner text!
> args of call = {{a}},{{b}}
> macro name = {{delay_macro.name}}
> via caller = {{delay_macro.caller}}
> {% endcall %}

### Version history

- 1.26 : Ported to CHICKEN 5
- 1.23 : Added built-in function allCombs
- 1.18 : Renamed groupBy to batch for compatibility with Jinja
- 1.15 : Added built-in function pad
- 1.13 : Added built-in function groupBy
- 1.12 : Ensure is_autoescape variable is present in default template environment
- 1.9 : Allow negative numbers in lexer
- 1.8 : Using right associativity for dot operator; added first builtin
- 1.6 : Extended object lookup to support recursive lookups, e.g. x.y.z; added dict builtin
- 1.5 : Extended sexp->tvalue to support tvalues inside the sexp
- 1.4 : Bug fixes in parsing of unary operators; added sexpr->tvalue and tvalue->sexpr [thanks to Matt Gushee for bug reports]
- 1.3 : Bug fixes in parsing of two-character operators
- 1.0 : Initial release

### License

>
> Copyright 2012-2018 Ivan Raikov. 
> 
> This program is free software: you can redistribute it and/or modify
> it under the terms of the GNU General Public License as published by
> the Free Software Foundation, either version 3 of the License, or
> (at your option) any later version.
> 
> This program is distributed in the hope that it will be useful, but
> WITHOUT ANY WARRANTY; without even the implied warranty of
> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
> General Public License for more details.
> 
> A full copy of the GPL license can be found at
> <http://www.gnu.org/licenses/>.
>