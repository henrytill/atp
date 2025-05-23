#!/usr/bin/env bash

export TERM=dumb
export DEBIAN_FRONTEND=noninteractive
export OPAMCONFIRMLEVEL=unsafe-yes

opam install --deps --with-test --with-doc .
