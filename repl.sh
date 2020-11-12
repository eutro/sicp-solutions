#!/bin/sh

rm -rf docs
export SICP_REPL=true
scribble --htmls --dest-name docs chapters/main.scrbl
unset SICP_REPL
