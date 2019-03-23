# Copyright (c) 2018      Magit contributors
# Copyright (c) 2017-2018 Flycheck contributors
# Copyright (c) 2015-2016 Sebastian Wiesner <swiesner@lunaryorn.com>

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.EMACS_VERSION ?= 25.3


MAKE_JOBS ?= 2

# Tear the version apart
VERSION_PARTS := $(subst -, ,$(EMACS_VERSION))
VERSION_PART := $(word 1,$(VERSION_PARTS))
PRE_RELEASE_PART := $(word 2,$(VERSION_PARTS))
MAJOR_VERSION := $(word 1,$(subst ., ,$(EMACS_VERSION)))
# Whether the version is a release candidate
PRETEST ?= $(findstring rc,$(PRE_RELEASE_PART))

# Build a minimal Emacs with no special flags, to build as fast as possible
ifndef EMACSCONFFLAGS
EMACSCONFFLAGS := --with-modules --with-gnutls=no --prefix=/c/emacs CFLAGS='-O2 -march=native' CXXFLAGS='-O2 -march=native'
endif

# Clone Emacs from the Github mirror because it's way faster than upstream
EMACS_GIT_URL = https://github.com/emacs-mirror/emacs.git
ifeq ($(PRETEST),)
EMACS_FTP_URL = "https://ftp.gnu.org/gnu/emacs"
else
EMACS_FTP_URL = "http://alpha.gnu.org/pub/gnu/emacs/pretest"
endif
EMACS_TAR_URL = $(EMACS_FTP_URL)/emacs-$(EMACS_VERSION).tar.xz

# If it's an RC the real reported Emacs version is the version without the
# prerelease postfix.  Otherwise it's just the version that we get.
ifneq ($(PRETEST),)
REPORTED_EMACS_VERSION = $(VERSION_PART)
else
REPORTED_EMACS_VERSION = $(EMACS_VERSION)
endif

CONFIGUREFLAGS = --quiet --enable-silent-rules --prefix="$(HOME)"

.PHONY: download_emacs_stable clone_emacs_snapshot
.PHONY: configure_emacs install_emacs

download_emacs_stable:
	mkdir tmp
	echo "Download Emacs $(EMACS_VERSION) from $(EMACS_TAR_URL)"
	curl -o "tmp/emacs-$(EMACS_VERSION).tar.xz" "$(EMACS_TAR_URL)"
	tar xf "tmp/emacs-$(EMACS_VERSION).tar.xz" -C tmp
	mkdir -p `dirname "$(EMACS_DIR)"`
	mv tmp/emacs-$(REPORTED_EMACS_VERSION) "$(EMACS_DIR)"

clone_emacs_snapshot:
	echo "Clone Emacs from Git"
	git clone -q --depth=1 '$(EMACS_GIT_URL)' $(EMACS_DIR)
	cd $(EMACS_DIR) && ./autogen.sh

configure_emacs:
	echo "Configure Emacs $(EMACS_VERSION)"
	cd "$(EMACS_DIR)" && ./configure $(CONFIGUREFLAGS) $(EMACSCONFFLAGS) $(SILENT)

EMACS_DIR = tmp/emacs
ifeq ($(EMACS_VERSION),snapshot)
configure_emacs: clone_emacs_snapshot
else
configure_emacs: download_emacs_stable
endif

install_emacs: configure_emacs
	echo "Install Emacs $(EMACS_VERSION)"
	make -C "$(EMACS_DIR)" -j$(MAKE_JOBS) install prefix=/c/emacs
