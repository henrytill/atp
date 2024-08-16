.SUFFIXES:

OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
MENHIR = menhir
OCAMLLEX = ocamllex
OCAMLFIND = ocamlfind

INCLUDES =
OCAMLCFLAGS = $(INCLUDES) -bin-annot -g
OCAMLOPTFLAGS = $(INCLUDES) -bin-annot -g
OCAMLFINDFLAGS =

GENERATED =
GENERATED += lib/intro/intro__parser.ml
GENERATED += lib/intro/intro__parser.mli
GENERATED += lib/intro/intro__lexer.ml
GENERATED += lib/prop_logic/prop_logic__parser.ml
GENERATED += lib/prop_logic/prop_logic__parser.mli
GENERATED += lib/prop_logic/prop_logic__lexer.ml

INTRO_OBJS =
INTRO_OBJS += lib/intro/intro__syntax.cmo
INTRO_OBJS += lib/intro/intro__semantics.cmo
INTRO_OBJS += lib/intro/intro__parser.cmo
INTRO_OBJS += lib/intro/intro__lexer.cmo
INTRO_OBJS += lib/intro/intro__input.cmo
INTRO_OBJS += lib/intro/intro.cmo

INTRO_SUBS = $(filter lib/intro/intro__%,$(INTRO_OBJS))

PROP_LOGIC_OBJS =
PROP_LOGIC_OBJS += lib/prop_logic/prop_logic__syntax.cmo
PROP_LOGIC_OBJS += lib/prop_logic/prop_logic__semantics.cmo
PROP_LOGIC_OBJS += lib/prop_logic/prop_logic__parser.cmo
PROP_LOGIC_OBJS += lib/prop_logic/prop_logic__lexer.cmo
PROP_LOGIC_OBJS += lib/prop_logic/prop_logic__input.cmo
PROP_LOGIC_OBJS += lib/prop_logic/prop_logic.cmo

PROP_LOGIC_SUBS = $(filter lib/prop_logic/prop_logic__%,$(PROP_LOGIC_OBJS))

ARCHIVES =
ARCHIVES += lib/intro/intro.cma
ARCHIVES += lib/prop_logic/prop_logic.cma

TESTS =
TESTS += test/test_intro.byte
TESTS += test/test_prop_logic.byte

-include config.mk

.PHONY: all
all: $(ARCHIVES) bin/main.byte

.PHONY: check
check: $(TESTS)
	./test/test_intro.byte
	./test/test_prop_logic.byte

# intro

lib/intro/intro.cma: lib/intro/intro.cmi $(INTRO_OBJS)
	$(OCAMLC) -a $(INTRO_OBJS) -o $@

lib/intro/intro.cmi: lib/intro/intro.mli
	$(OCAMLC) $(OCAMLCFLAGS) -no-alias-deps -w -49 -c $<

$(INTRO_OBJS): INCLUDES += -I lib/intro

$(INTRO_SUBS): OCAMLCFLAGS += -no-alias-deps -open Intro

lib/intro/intro__lexer.ml: lib/intro/intro__parser.mly

# prop_logic

lib/prop_logic/prop_logic.cma: lib/prop_logic/prop_logic.cmi $(PROP_LOGIC_OBJS)
	$(OCAMLC) -a $(PROP_LOGIC_OBJS) -o $@

lib/prop_logic/prop_logic.cmi: lib/prop_logic/prop_logic.mli
	$(OCAMLC) $(OCAMLCFLAGS) -no-alias-deps -w -49 -c $<

$(PROP_LOGIC_OBJS): INCLUDES += -I lib/prop_logic

$(PROP_LOGIC_SUBS): OCAMLCFLAGS += -no-alias-deps -open Prop_logic

lib/prop_logic/prop_logic__lexer.ml: lib/prop_logic/prop_logic__parser.mly

# main

bin/main.byte: INCLUDES += -I lib/intro -I lib/prop_logic
bin/main.byte: lib/intro/intro.cma lib/prop_logic/prop_logic.cma bin/main.ml
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ $^

# tests

test/test_intro.byte: OCAMLFINDFLAGS += -linkpkg -package alcotest
test/test_intro.byte: INCLUDES += -I lib/intro
test/test_intro.byte: lib/intro/intro.cma test/test_intro.ml
	$(OCAMLFIND) $(OCAMLC) $(OCAMLCFLAGS) -o $@ $(OCAMLFINDFLAGS) $^

test/test_prop_logic.byte: OCAMLFINDFLAGS += -linkpkg -package alcotest
test/test_prop_logic.byte: INCLUDES += -I lib/prop_logic
test/test_prop_logic.byte: lib/prop_logic/prop_logic.cma test/test_prop_logic.ml
	$(OCAMLFIND) $(OCAMLC) $(OCAMLCFLAGS) -o $@ $(OCAMLFINDFLAGS) $^

# general

%.ml: %.mll
	$(OCAMLLEX) $<

%.ml %.mli: %.mly
	$(MENHIR) --explain $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

%.cmo: %.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

$(PREFIX)/bin:
	mkdir -p $@

.PHONY: install
install: bin/main.byte | $(PREFIX)/bin
	install -m 755 bin/main.byte $(PREFIX)/bin/main.byte

.PHONY: clean
clean:
	rm -f $(INTRO_OBJS) $(INTRO_OBJS:.cmo=.cmt)
	rm -f $(INTRO_OBJS:.cmo=.cmi) $(INTRO_OBJS:.cmo=.cmti)
	rm -f $(PROP_LOGIC_OBJS) $(PROP_LOGIC_OBJS:.cmo=.cmt)
	rm -f $(PROP_LOGIC_OBJS:.cmo=.cmi) $(PROP_LOGIC_OBJS:.cmo=.cmti)
	rm -f $(ARCHIVES)
	rm -f bin/main.byte

.PHONY: distclean
distclean: clean
	rm -f .depend
	rm -f $(GENERATED)

.depend: GNUmakefile $(GENERATED)
	@printf "# -*- mode: makefile; -*-\n\n" > $@
	@printf "# intro\n" >> $@
	$(OCAMLDEP) -I lib/intro -map lib/intro/intro.mli -open Intro $(INTRO_OBJS:.cmo=.ml) >> $@
	@printf "\n# prop_logic\n" >> $@
	$(OCAMLDEP) -I lib/prop_logic -map lib/prop_logic/prop_logic.mli -open Prop_logic $(PROP_LOGIC_OBJS:.cmo=.ml) >> $@

include .depend
