LISP ?= sbcl

build:
	$(LISP) --load hm.asd \
	     --eval '(ql:quickload :hm)' \
		--eval '(asdf:make :hm)' \
		--eval '(quit)'
