FROM mcr.microsoft.com/devcontainers/base:bookworm

ARG OPAM_VERSION=2.3.0
ARG OPAM_BINARY="opam-${OPAM_VERSION}-x86_64-linux"
ARG OPAM_BINARY_SIG="${OPAM_BINARY}.sig"
ARG OPAM_BINARY_URL="https://github.com/ocaml/opam/releases/download/${OPAM_VERSION}/${OPAM_BINARY}"
ARG OPAM_BINARY_SIG_URL="${OPAM_BINARY_URL}.sig"
ARG DEFAULT_COMPILER=ocaml-base-compiler.5.3.0

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update -qq && \
    apt-get install -yqq build-essential pkg-config libgmp-dev darcs mercurial rsync

WORKDIR /tmp

RUN wget -q -O - https://opam.ocaml.org/opam-dev-pubkey.pgp | gpg --import -

RUN wget -q $OPAM_BINARY_URL

RUN wget -q $OPAM_BINARY_SIG_URL

RUN gpg --verify $OPAM_BINARY_SIG

RUN mv $OPAM_BINARY /usr/local/bin/opam

RUN chmod +x /usr/local/bin/opam

USER vscode

RUN opam init --disable-sandboxing --auto-setup -c $DEFAULT_COMPILER -q

RUN opam install -yq ocaml-lsp-server ocamlformat odig dune
