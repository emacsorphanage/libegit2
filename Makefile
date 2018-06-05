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

test: EMACS_OPTS=-L build/ -l libegit2
test: emake.el build/libegit2.so
	$(EMAKE) test ert

clean:
	rm -rf build/ libgit2/

emake.el:
	wget 'https://raw.githubusercontent.com/vermiculus/emake.el/master/emake.el'

emacs-travis.mk:
	wget 'https://raw.githubusercontent.com/flycheck/emacs-travis/master/emacs-travis.mk'
ifeq ($(TRAVIS_OS_NAME),osx)
	cp emacs-travis.mk /tmp/emacs-travis.mk
	cat /tmp/emacs-travis.mk | sed 's/configure_emacs: install_gnutls//' > emacs-travis.mk
endif

ifeq ($(TRAVIS_OS_NAME),osx)
emacs: emacs-travis.mk
	brew upgrade gnutls
	$(MAKE) -f emacs-travis.mk install_emacs
	mkdir -p $(HOME)/bin
	ln -s $(HOME)/emacs/$(EMACS_VERSION)/nextstep/Emacs.app/Contents/MacOS/Emacs $(HOME)/bin/emacs
else
emacs: emacs-travis.mk
	$(MAKE) -f emacs-travis.mk install_emacs
endif
