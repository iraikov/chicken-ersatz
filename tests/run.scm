

(import scheme (chicken base) (chicken string) test ersatz datatype)

(define kwargs '())

(define (tval-equal? t1 t2)
  (cases tvalue (eq-eq t1 t2)
         (Tbool (ret) ret)
         (else (error 'tval-equal "invalid value"))))

(define (alist->models xs)
  (map (lambda (x) (cons (car x) (sexpr->tvalue (cdr x)))) xs))

(lexer-trace #f)

(test-group "runtime test"


  (test-group "tvalue string representation"
	      (test "a"   (->string (Tstr "a")))
	      (test "1"   (->string (Tint 1)))
	      (test "1.0"  (->string (Tfloat 1.0)))
	      (test "1.2"  (->string (Tfloat 1.2)))
	      (test "<list>"  (->string (Tlist (list (Tint 0) (Tint 1)))))
	      (test "<obj>"  (->string (Tobj (list (cons 'name (Tstr "value"))))))
	      )

  (test-group "arithmetic and logic"
	      (test (Tint 2)     (op-plus (Tint 1) (Tint 1)))
	      (test (Tfloat 2.0) (op-plus (Tfloat 1.0) (Tfloat 1.0)))
	      (test (Tint 0)     (op-minus (Tint 1) (Tint 1)))
	      (test (Tint 2)     (op-minus (Tint 1) (Tint -1)))
	      (test (Tfloat -1.0) (op-minus (Tint 0) (Tfloat 1.0)))
	      (test (Tfloat 1.0)    (op-minus (Tint 1) (Tfloat 0.0)))
	      (test-assert (tval-equal? (op-abs (Tint -1) kwargs) (Tint 1)))
	      (test-assert (tval-equal? (op-abs (Tint 1) kwargs) (Tint 1)))
	      (test-assert (tval-equal? (op-round (Tstr "floor") (Tfloat 1.5) kwargs) 
					(Tfloat 1.0)))
	      (test-assert (tval-equal? (op-round (Tstr "ceil") (Tfloat 1.5) kwargs) 
					(Tfloat 2.0)))
	      (test-assert (tval-equal? (op-range (Tint 0) (Tint 2) kwargs) 
					(Tlist (list (Tint 0) (Tint 1) (Tint 2)))))
	      (test-assert (tval-equal? (op-range (Tint 2) (Tint 0) kwargs) 
					(Tlist (list (Tint 2) (Tint 1) (Tint 0)))))
	      (test-assert (tval-equal? (op-range (Tint 2012) (Tint 2006) kwargs) 
					(Tlist (list (Tint 2012) (Tint 2011) (Tint 2010) (Tint 2009) 
						     (Tint 2008) (Tint 2007) (Tint 2006)))))
;;	      (test-assert (tval-equal? (op-sum (Tlist (Tint 0) (Tint 1) (Tint 2) kwargs))
;;					(Tint 3)))
;;	      (test-assert (tval-equal? (op-sum (Tlist (Tint 0) (Tint 1) (Tfloat 2.1) kwargs))
;;					(Tfloat 3.1)))


	      (test-assert (tval-equal? (op-times (Tint 0) (Tint 1)) (Tint 0)))
	      (test-assert (tval-equal? (op-times (Tint 1) (Tint 1)) (Tint 1)))
	      (test-assert (tval-equal? (op-times (Tint 2) (Tint 2)) (Tint 4)))
	      (test-assert (tval-equal? (op-times (Tint 2) (Tint 3)) (Tint 6)))
	      (test-assert (tval-equal? (op-times (Tfloat 1.0) (Tint 2)) (Tfloat 2.0)))
	      (test-assert (tval-equal? (op-times (Tfloat 2.0) (Tfloat 2.0)) (Tfloat 4.0)))
	      (test-assert (tval-equal? (op-times (Tfloat 2.0) (Tfloat 3.0)) (Tfloat 6.0)))
	      (test-assert (tval-equal? (op-times (Tfloat 0.0) (Tfloat 2.0)) (Tfloat 0.0)))
	      (test-assert (tval-equal? (op-times (Tfloat 0.0) (Tint 1)) (Tfloat 0.0)))

	      (test-assert (tval-equal? (op-power (Tint 2) (Tint -1)) (Tfloat 1.0)))
	      (test-assert (tval-equal? (op-power (Tint 2) (Tint 0)) (Tfloat 1.0)))
	      (test-assert (tval-equal? (op-power (Tint 2) (Tint 1)) (Tfloat 2.0)))
	      (test-assert (tval-equal? (op-power (Tint 2) (Tint 10)) (Tfloat 1024.0)))

	      (test-assert (tval-equal? (op-div (Tint 4) (Tint 2)) (Tint 2)))
	      (test-assert (tval-equal? (op-div (Tfloat 4.0) (Tint 2)) (Tfloat 2.0)))

	      (test-assert (tval-equal? (op-mod (Tint 4) (Tint 3)) (Tint 1)))
	      (test-assert (tval-equal? (op-mod (Tint 4) (Tint 1)) (Tint 0)))

	      (test-assert (tval-equal? (op-and (Tbool #t) (Tbool #t)) (Tbool #t)))
	      (test-assert (tval-equal? (op-and (Tbool #t) (Tbool #f)) (Tbool #f)))
	      (test-assert (tval-equal? (op-and (Tbool #f) (Tbool #t)) (Tbool #f)))
	      (test-assert (tval-equal? (op-and (Tbool #f) (Tbool #f)) (Tbool #f)))

	      (test-assert (tval-equal? (op-or (Tbool #t) (Tbool #t)) (Tbool #t)))
	      (test-assert (tval-equal? (op-or (Tbool #t) (Tbool #f)) (Tbool #t)))
	      (test-assert (tval-equal? (op-or (Tbool #f) (Tbool #t)) (Tbool #t)))
	      (test-assert (tval-equal? (op-or (Tbool #f) (Tbool #f)) (Tbool #f)))


	      (test-assert (tval-equal? (op-toint (Tint 1) kwargs) (Tint 1)))
	      (test-assert (tval-equal? (op-toint (Tfloat 1.0) kwargs) (Tint 1)))
	      (test-assert (tval-equal? (op-tofloat (Tint 1) kwargs) (Tfloat 1.0)))
	      (test-assert (tval-equal? (op-tofloat (Tfloat 1.0) kwargs) (Tfloat 1.0)))
	      )


  (let ((lst1 (Tlist (list (Tint 0) (Tint 1) (Tint 2))))
	(lst2 (Tlist (list (Tint 0) (Tint 1) (Tint 2))))
	(lst3 (Tlist (list (Tint 0) (Tint 1) (Tint 3))))
	(lst4 (Tlist (list (Tint 0) (Tint 1)))))
    (test-group "list equality"
		(test-assert (tval-equal? (Tbool #t) (list-same lst1 lst2)))
		(test-assert (tval-equal? (Tbool #f) (list-same lst1 lst3)))
		(test-assert (tval-equal? (Tbool #f) (list-same lst1 lst4)))))
			     
  (let ((obj1 (Tobj (list (cons 'name (Tstr "john"))
			  (cons 'age  (Tint 20)))))
	(obj2 (Tobj (list (cons 'name (Tstr "john"))
			  (cons 'age  (Tint 20)))))
	(obj3 (Tobj (list (cons 'name (Tstr "mary"))
			  (cons 'age  (Tint 22)))))
	(obj4 (Tobj (list (cons 'age (Tint 20))
			  (cons 'name (Tstr "john"))))))
    (test-group "object equality"
		(test-assert (tval-equal?  (Tbool #t) (obj-same obj1 obj2)))
		(test-assert (tval-equal?  (Tbool #f) (obj-same obj1 obj3)))
		(test-assert (tval-equal? (Tbool #t) (obj-same obj1 obj4))))
    )


  (test-group "eq-eq"
	      (test-assert (tval-equal? (eq-eq (Tint 1) (Tint 1)) (Tbool #t)))
	      (test-assert (tval-equal? (eq-eq (Tint 1) (Tfloat 1.0)) (Tbool #f)))
	      (test-assert (tval-equal? (eq-eq (Tfloat 1.0) (Tfloat 1.0)) (Tbool #t)))
	      (test-assert (tval-equal? (eq-eq (Tstr "aaa") (Tstr "aaa")) (Tbool #t)))
	      (test-assert (tval-equal? (eq-eq (Tstr "aaa") (Tstr "bbb")) (Tbool #f)))
	      (test-assert (tval-equal? (eq-eq (Tstr "日本語") (Tstr "日本語")) (Tbool #t)))
	      (test-assert (tval-equal? (eq-eq (Tstr "日本語") (Tstr "英語")) (Tbool #f)))
	      (test-assert (tval-equal? (eq-eq (Tstr "aaa") (Tint 0)) (Tbool #f)))
   )

  (test-group "string operations"
	      (test-assert "upper" (tval-equal? (op-upper (Tstr "aaa") kwargs) (Tstr "AAA")))
	      (test-assert "lower" (tval-equal? (op-lower (Tstr "AAA") kwargs) (Tstr "aaa")))
	      (test-assert "join" (tval-equal? (op-join (Tstr ",") (Tlist (list (Tstr "a") (Tstr "b"))) kwargs) (Tstr "a,b")))
	      (test-assert "substring" (tval-equal? (op-substring (Tint 0) (Tint 1) (Tstr "hello") kwargs) (Tstr "h")))
	      (test-assert "substring" (tval-equal? (op-substring (Tint 4) (Tint 1) (Tstr "hello") kwargs) (Tstr "o")))
	      (test-assert "substring" (tval-equal? (op-substring (Tint 0) (Tint 2) (Tstr "hello") kwargs) (Tstr "he")))
	      (test-assert "substring" (tval-equal? (op-substring (Tint 0) (Tint 2) (Tstr "日本語") kwargs) (Tstr "日本")))

	      (test-assert "replace" (tval-equal? (op-replace (Tstr "t") (Tstr "d") (Tstr "test") kwargs) (Tstr "desd")))
	      (test-assert "replace" (tval-equal? (op-replace (Tstr "te") (Tstr "ta") (Tstr "test") kwargs) (Tstr "tast")))
	      (test-assert "replace" (tval-equal? (op-replace (Tstr "日") (Tstr "英") (Tstr "日語") kwargs) (Tstr "英語")))

	      (test-assert "truncate"
			   (tval-equal? (op-truncate (Tint 3) (Tstr "123456789") kwargs) (Tstr "123")))

	      (test-assert "capitalize"
			   (tval-equal? (op-capitalize (Tstr "car") kwargs) (Tstr "Car")))

	      (test-assert "escape-html"
			   (tval-equal?
			    (Tstr "&lt;script&gt;") 
			    (op-escape-html (Tstr "<script>") kwargs)
			    ))


	      (test-assert "wordcount" 
			   (tval-equal?
			    (op-wordcount (Tstr "xy yz zz") kwargs) (Tint 3)))
	      
	      (test-assert "wordcount" 
			   (tval-equal?
			    (op-wordcount (Tstr "英語 日語") kwargs) (Tint 2)))

	      (test-assert "urlize" 
			   (tval-equal?
			    (op-urlize (Tstr "go to http://yahoo.co.jp" ) kwargs) 
			    (Tstr "go to <a href='http://yahoo.co.jp'>http://yahoo.co.jp</a>")))

	      (test-assert "title" 
			   (tval-equal?
			    (op-title (Tstr "this is a title" ) kwargs) 
			    (Tstr "This Is A Title")))

	      (test-assert "striptags" 
			   (tval-equal?
			    (op-striptags (Tstr "<p class='indent'>xxx</p> yyy <b>zzz</b>" ) kwargs) 
			    (Tstr "xxx yyy zzz")))

	      (test-assert "trim" 
			   (tval-equal?
			    (op-trim (Tstr "   this is a test   " ) kwargs) 
			    (Tstr "this is a test")))

	      (test-assert "pad" 
			   (tval-equal?
			    (op-pad (Tint 5) (Tstr "this is a test" ) kwargs) 
			    (Tstr "     this is a test")))

	      )


  (test-group "sorting"

	      (let ((lst  (Tlist (list (Tint 3) (Tint 1) (Tint 2)))))
		(test-assert (tval-equal? (op-sort lst kwargs)
					  (Tlist (list (Tint 1) (Tint 2) (Tint 3))))))

	      (let ((lst (Tlist (list (Tfloat 3.0) (Tfloat 1.0) (Tfloat 2.0)))))
		(test-assert (tval-equal? (op-sort lst kwargs)
					  (Tlist (list (Tfloat 1.0) (Tfloat 2.0) (Tfloat 3.0))))))

	      (let ((lst (Tlist (list (Tstr "baba") (Tstr "aa") (Tstr "kaka")))))
		(test-assert (tval-equal? (op-sort lst kwargs)
					  (Tlist (list (Tstr "aa") (Tstr "baba") (Tstr "kaka"))))))
	      )


  (test-group "length"
	      (test-assert (tval-equal? (op-length (Tstr "test") kwargs) (Tint 4)))
	      (test-assert (tval-equal? (op-length (Tstr "日本語") kwargs) (Tint 3)))
	      (test-assert (tval-equal? (op-length (Tlist (list (Tint 0) (Tint 1))) kwargs) (Tint 2)))
	      )

  (test-assert "reverse"  (tval-equal? (op-reverse (Tlist (list (Tint 0) (Tint 1) (Tint 2))) kwargs)
				       (Tlist (list (Tint 2) (Tint 1) (Tint 0)))))

  
  (test-assert "slice"  (tval-equal? (op-slice (Tint 2) (Tlist (list (Tint 0) (Tint 1) (Tint 2) (Tint 3) (Tint 4))) kwargs)
				     (Tlist (list
					     (Tlist (list (Tint 0) (Tint 1)))
					     (Tlist (list (Tint 2) (Tint 3)))
					     (Tlist (list (Tint 4)))))))

  (test-assert "last" (tval-equal? (op-last (Tlist (list (Tint 0) (Tint 1) (Tint 2))) kwargs)
				   (Tint 2)))

  (test-assert "default" (tval-equal? (op-default (Tstr "hello") (Tnull) kwargs) (Tstr "hello")))

  (test-assert "list" (tval-equal? (op-list (Tstr "test") kwargs)
				   (Tlist (list (Tstr "t") (Tstr "e") (Tstr "s") (Tstr "t")))))

  (test-assert "sublist" (tval-equal? (op-sublist (Tint 1) (Tint 3) (Tlist (list (Tint 0) (Tint 1) (Tint 2) (Tint 3))) kwargs)
				      (Tlist (list (Tint 1) (Tint 2) (Tint 3)))))
  
  (test-assert "batch" (tval-equal? (op-batch (Tint 3) (Tlist (list (Tint 0) (Tint 1) (Tint 2) (Tint 3))) kwargs)
                                    (Tlist (list (Tlist (list (Tint 0) (Tint 1) (Tint 2))) (Tlist (list (Tint 3)))))))

  (test-assert "expand and escape"
	       (let* ((script "<script>alert(1)</script>")
		      (output (from-string "{{danger}}" 
					   models: (alist->models `((danger . ,script)))))
		      )
		 (tval-equal? (Tstr output) (op-escape-html (Tstr script) kwargs))))

  (test-assert "safe expand"
	       (let* ((script "<script>alert(1)</script>")
		      (output (from-string "{{danger|safe}}" 
					   models: (alist->models `((danger . ,script)))))
		      )
		 (tval-equal? (Tstr output) (Tstr script))))

  (test-assert "expand with filter"
	       (let* ( 
                      (output (from-string "{{pi|int}}" 
					   models: (alist->models '((pi . 3.14)))))
		      )
		 (tval-equal? (Tstr output) (Tstr "3"))))

  (test-assert "if"
	       (let* ( (source "{% if x <= 1 %}one{% elseif x == 2 %}two{% else %}three{% endif %}") )
		 (and (tval-equal? (Tstr (from-string source
						      models: (alist->models '((x . 1)))))
				   (Tstr "one"))
		      (tval-equal? (Tstr (from-string source
						      models: (alist->models '((x . 2)))))
				   (Tstr "two"))
		      (tval-equal? (Tstr (from-string source
						      models: (alist->models '((x . 3)))))
				   (Tstr "three"))
		 )))
  (print (from-string "{% if not (x == 1) %}not one{% endif %}"
               models: (alist->models '((x . 2)))))
  (print (from-string "{% if not x == 1 %}not one{% endif %}"
               models: (alist->models '((x . 2)))))
  (test-assert "if"
	       (let* ( (source "{% if not x == 1 %}not one{% endif %}") )
		 (and (tval-equal? (Tstr (from-string source
						      models: (alist->models '((x . 2)))))
				   (Tstr "not one"))
		 )))


  (test 
#<<EOF
  <div class="blog-post">
    <h3>Post One title</h3>
    <div class="post-body">
     Post One body    </div>
  </div>
  <div class="blog-post">
    <h3>Post Two title</h3>
    <div class="post-body">
     Post Two body    </div>
  </div>

EOF

	       (let* ( (source #<<EOF
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
		 (from-string source models:
                                    (alist->models
                                   '((posts .  (((title . "Post One title")
                                                   (body  . "Post One body"))
                                                  ((title . "Post Two title")
                                                   (body  . "Post Two body"))
                                                  )))
                                      ))

))
   


  (test-assert "loop index/revindex"
	       
	       (and 
		(let* ( (source "{% for i in range(1,3) %}{{loop.index}}{% endfor %}" ))
		  (tval-equal? (Tstr (from-string source)) (Tstr "123")))
		(let* ( (source "{% for i in range(1,3) %}{{loop.index0}}{% endfor %}" ))
		  (tval-equal? (Tstr (from-string source)) (Tstr "012")))
		(let* ( (source "{% for i in range(1,3) %}{{loop.revindex}}{% endfor %}" ))
		  (tval-equal? (Tstr (from-string source)) (Tstr "321")))
		(let* ( (source "{% for i in range(1,3) %}{{loop.revindex0}}{% endfor %}" ))
		  (tval-equal? (Tstr (from-string source)) (Tstr "210")))

	       ))



  (test-assert "loop cycle"
	       (let* ( (source "{% for i in range(1,3) %}{{loop.cycle(\"foo\",\"bar\",\"test\")}}{% endfor %}" ))
		 (tval-equal? (Tstr (from-string source)) (Tstr "foobartest"))))

  (test-assert "loop first"
	       (let* ( (source "{% for i in range(1,3) %}{{loop.first}}{% endfor %}" ))
		 (tval-equal? (Tstr (from-string source)) (Tstr "truefalsefalse"))))

  (test-assert "loop last"
	       (let* ( (source "{% for i in range(1,3) %}{{loop.last}}{% endfor %}" ))
		 (tval-equal? (Tstr (from-string source)) (Tstr "falsefalsetrue"))))

  (test-assert "loop last"
	       (let* ( (source "{% for i in range(1,3) %}{{not loop.last}}{% endfor %}" ))
		 (tval-equal? (Tstr (from-string source)) (Tstr "truetruefalse"))))

  (test-assert "loop length"
	       (let* ( (source "{% for i in range(1,3) %}{{loop.length}}{% endfor %}" ))
		 (tval-equal? (Tstr (from-string source)) (Tstr "333"))))

  (test-assert "in"
	       (let* ( (source "{{ 'aa' in ['bb', 'aa', 'cc'] }}" ))
		 (tval-equal? (Tstr (from-string source)) (Tstr "true"))))


  (test-assert "is"
	       (let* ( (source "{{ 6 is divisibleby 3 }}" ))
		 (tval-equal? (Tstr (from-string source)) (Tstr "true"))))

  (test-assert "set"
	       (let* ( (source "{% set x = 'aa' %}{{ x }}"  ))
		 (tval-equal? (Tstr (from-string source)) (Tstr "aa"))))

  (test-assert "set/with"
	       (let* ( (source "{% set x = 'aa' %}{% with x = 'bb', y = 'cc' %}{{ x }}{{ y }}{% endwith %}{{ x }}{{ y }}"  ))
		 (tval-equal? (Tstr (from-string source)) (Tstr "bbccaa"))))

  (test-assert "set/with"
	       (let* ( (source "{% set x = '/home' %}{% with x = '/usr', y = '/bin' %}{{ x }}{{ y }}{% endwith %}{{ x }}{{ y }}"  ))
		 (tval-equal? (Tstr (from-string source)) (Tstr "/usr/bin/home"))))

  (test-assert "defined"
	       (let* ( (source "{% set obj = { age:10, name: 'aa' } %} {{ obj.age is defined }}"  ))
		 (tval-equal? (Tstr (from-string source)) (Tstr " true"))))

  (test-assert "defined"
	       (let* ( (source "{% set obj = { age:10, name: 'aa' } %} {{ obj['name'] is defined }}"  ))
		 (tval-equal? (Tstr (from-string source)) (Tstr " true"))))

  (test-assert "extends"
	       (tval-equal? (Tstr (from-file "extends.tmpl" 
					     env: (template-std-env search-path: '("tests/tmpl" "tmpl"))))
			    (Tstr "extended")))

  (test-assert "include"
	       (tval-equal? (Tstr (from-file "include.tmpl" 
					     env: (template-std-env search-path: '("tests/tmpl" "tmpl"))))
			    (Tstr "this is included\n")))
  
  (let ((macro-three-words #<<EOF
{% macro three_words(one,two,three) %}
{{one}} {{two}} {{three}}{{caller (' by','chicken','scheme')}}
{% endmacro %}
EOF
))


  (test-assert "macro"
	       (tval-equal? 
                  (Tstr (from-string
			 (string-append macro-three-words
					"{{ three_words(\"this\", \"is\", \"it!\") }}")))
                  (Tstr "this is it!")))
       
  (test-assert "caller"
	       (tval-equal? 
                  (Tstr (from-string
			 (string-append macro-three-words
					(string-append
					 "{% call(a,b,c) three_words('this', 'is', 'it!') %}"
					 "{{a}} {{b}} {{c}}";
					 "{% endcall %}")
					)))
                  (Tstr "this is it! by chicken scheme")))

)

   (test-assert "filter"
       (tval-equal? 
         (Tstr (from-string "{% filter upper %}must be upper{% endfilter %}"))
         (Tstr "MUST BE UPPER")))


   (test-assert "set"
       (tval-equal? 
         (Tstr (from-string "{% set x = \"test\" %}{{x}}"))
         (Tstr "test")))

   (test-assert "make-lexer"
	       (let* ((lexer (make-lexer 
                              begin-expand: "%{{" end-expand: "%}}" 
                              compile: #f))
                      (env (template-std-env lexer: lexer))
                      (script "<script>alert(1)</script>")
		      (output (from-string "This is some text. %{{danger%}}" 
					   env: env
					   models: (list (cons 'danger (Tstr script)))))
		      )
		 (tval-equal? (Tstr output) 
                              (op-escape-html (Tstr (string-append "This is some text. " script)) kwargs))))

#|

   (test-assert "make-lexer (compiled)"
	       (let* ((lexer (make-lexer begin-expand: "%{{" end-expand: "%}}" compile: #t))
                      (env (template-std-env lexer: lexer))
                      (script "<script>alert(1)</script>")
		      (output (from-string "%{{danger%}}" 
					   env: env
					   models: (list (cons 'danger (Tstr script)))))
		      )
		 (tval-equal? (Tstr output) (op-escape-html (Tstr script) kwargs))))
|#

     
)

(print (from-file "cheatsheet.tmpl" 
		  env: (template-std-env search-path: '("tests/tmpl" "tmpl"))))
 
