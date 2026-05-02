SBCL ?= /usr/bin/sbcl
LISP := $(SBCL) --non-interactive --eval '(push (truename ".") asdf:*central-registry*)'

cave: src/*.lisp *.asd
	$(LISP) --eval '(asdf:make :cave)'
	chmod +x cave

.PHONY: load lint clean

load:
	$(LISP) --eval '(asdf:load-system :cave)' --eval '(format t "~%OK~%")'

lint:
	ocicl lint src/*.lisp

clean:
	rm -rf *~ cave
