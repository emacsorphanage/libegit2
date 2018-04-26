#!/usr/bin/env bash

git clone https://github.com/ubolonton/evm.git $HOME/.evm
evm config path /tmp
evm install emacs-24.3-travis --use --skip
