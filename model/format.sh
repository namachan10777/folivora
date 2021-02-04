#!/bin/sh

cd $(dirname $0)

find bin -type f -name "*.ml" | xargs ocamlformat -i
find lib -type f -name "*.ml" | xargs ocamlformat -i
find test -type f -name "*.ml" | xargs ocamlformat -i
