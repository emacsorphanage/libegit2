EMAKE_SHA1       ?= 1b23379eb5a9f82d3e2d227d0f217864e40f23e0
PACKAGE_BASENAME := libgit

emake.mk:
	wget "https://raw.githubusercontent.com/vermiculus/emake.el/$(EMAKE_SHA1)/emake.mk"

-include emake.mk

libgit2:                        ## pull down libgit2
	git submodule init
	git submodule update

build/libegit2.so: libgit2      ## build the module
	mkdir -p build
	cd build && cmake .. -DCMAKE_BUILD_TYPE=Debug && make

test: EMACS_ARGS += -L build/ -l libegit2
test: build/libegit2.so test-ert ## run tests

clean:                          ## removes build directories
	rm -rf build/
