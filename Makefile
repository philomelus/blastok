PACKAGE := blastok

.PHONY: all clean build

all: clean build

build: clean
	sbcl --eval "(proclaim '(optimize (compilation-speed 0) (debug 3) (safety 3) (space 0) (speed 0)))" \
		--eval '(ql:quickload :$(PACKAGE))' \
		--eval "(sb-ext:save-lisp-and-die \"$(PACKAGE)\" :toplevel #'$(PACKAGE):main :executable t)"

clean:
	-rm -f $(PACKAGE)

