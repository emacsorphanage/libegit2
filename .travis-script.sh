#!/usr/bin/env bash

mkdir -p build
cd build
cmake ..
make

cd ..
cask exec ert-runner
