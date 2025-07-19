PACKAGE := blastok

.PHONY: all clean build

all: clean build

build: clean
	sbcl --eval '(ql:quickload :$(PACKAGE))' \
		--eval "(sb-ext:save-lisp-and-die \"$(PACKAGE)\" :toplevel #'$(PACKAGE):main :executable t)"

clean:
	-rm -f $(PACKAGE)

