EENVS  = PACKAGE_FILE="libgit.el"
EENVS += PACKAGE_LISP="libgit.el"
EENVS += PACKAGE_TESTS="$(wildcard test/*.el)"
EENVS += PACKAGE_ARCHIVES="gnu"
EMAKE  = $(EENVS) emacs -batch -l emake.el $(EMACS_OPTS) --eval "(emake (pop argv))"

.PHONY: emacs clean test

libgit2:
	git submodule init
	git submodule update

build/libegit2.so: libgit2
	mkdir build
	cd build && cmake .. && make

test: EMACS_OPTS=-L build/ --eval "(require 'libegit2)"
test: emake.el build/libegit2.so
	$(EMAKE) test ert

clean:
	rm -rf build/ libgit2/

emake.el:
	wget 'https://raw.githubusercontent.com/vermiculus/emake.el/master/emake.el'
