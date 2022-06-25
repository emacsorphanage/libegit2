BUILD_OPTIONS=-DCMAKE_BUILD_TYPE=Debug

ifeq '$(findstring ;,$(PATH))' ';'
    UNAME := Windows
else
    UNAME := $(shell uname 2>/dev/null || echo Unknown)
    UNAME := $(patsubst CYGWIN%,Cygwin,$(UNAME))
    UNAME := $(patsubst MSYS%,MSYS,$(UNAME))
    UNAME := $(patsubst MINGW%,MSYS,$(UNAME))
endif

ifeq ($(UNAME),MSYS)
	BUILD_OPTIONS+= -G "MSYS Makefiles"
endif

-include .config.mk

PKG = libgit

ELS  = $(PKG).el
ELCS = $(ELS:.el=.elc)

EMACS      ?= emacs
EMACS_ARGS ?=

LOAD_PATH  ?= -L . -L build

.PHONY: test libgit2 submodule-update

all: lisp

help:
	$(info make all               - build everything)
	$(info make module            - generate module)
	$(info make lisp              - generate byte-code and autoloads)
	$(info make submodule-init    - update the submodule)
	$(info make submodule-update  - update the submodule)
	$(info make test              - run tests)
	$(info make clean             - remove generated files)
	@printf "\n"

module: build/libegit2.so

build/libegit2.so: libgit2
	@printf "Building $<\n"
	@mkdir -p build
	@cd build && cmake .. $(BUILD_OPTIONS) && make

lisp: $(ELCS) loaddefs module

loaddefs: $(PKG)-autoloads.el

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH) -f batch-byte-compile $<

test: libgit.elc build/libegit2.so
	$(EMACS) -Q --batch $(LOAD_PATH) -l libgit \
	  $(addprefix -l test/,$(shell ls test)) \
	  -f ert-run-tests-batch-and-exit

submodule-init: libgit2

submodule-update:
	@git submodule update

libgit2:
	@git submodule update --init

CLEAN  = $(ELCS) $(PKG)-autoloads.el build

clean:
	@printf "Cleaning...\n"
	@rm -rf $(CLEAN)

$(PKG)-autoloads.el: $(ELS)
	@printf " Creating $@\n"
	@$(EMACS) -Q --batch -l autoload -l cl-lib --eval "\
(let ((file (expand-file-name \"$@\"))\
      (autoload-timestamps nil) \
      (backup-inhibited t)\
      (version-control 'never)\
      (coding-system-for-write 'utf-8-emacs-unix))\
  (write-region (autoload-rubric file \"package\" nil) nil file nil 'silent)\
  (cl-letf (((symbol-function 'progress-reporter-do-update) (lambda (&rest _)))\
            ((symbol-function 'progress-reporter-done) (lambda (_))))\
    (let ((generated-autoload-file file))\
      (update-directory-autoloads default-directory))))"
