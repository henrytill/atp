#!/usr/bin/env bash

export TERM=dumb
export OPAMCONFIRMLEVEL=yes

opam install --deps --with-test --with-doc .
