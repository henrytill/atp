.SUFFIXES:

OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
MENHIR = menhir
OCAMLLEX = ocamllex
OCAMLFIND = ocamlfind
PYTHON3 = python3

INSTALL = install
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL) -m 644

INCLUDES =
OCAMLCFLAGS = $(INCLUDES) -bin-annot -g
OCAMLOPTFLAGS = $(INCLUDES) -bin-annot -g
OCAMLFINDFLAGS =

OCAMLC_WHERE = $(shell ocamlc -where)

bindir = /bin
prefix = /usr/local
DESTDIR = $(prefix)

GENERATED =
GENERATED += lib/intro/intro__parser.ml
GENERATED += lib/intro/intro__parser.mli
GENERATED += lib/intro/intro__lexer.ml
GENERATED += lib/prop_logic/prop_logic__parser.ml
GENERATED += lib/prop_logic/prop_logic__parser.mli
GENERATED += lib/prop_logic/prop_logic__lexer.ml

INTRO_CMOS =
INTRO_CMOS += lib/intro/intro.cmo
INTRO_CMOS += lib/intro/intro__syntax.cmo
INTRO_CMOS += lib/intro/intro__semantics.cmo
INTRO_CMOS += lib/intro/intro__parser.cmo
INTRO_CMOS += lib/intro/intro__lexer.cmo
INTRO_CMOS += lib/intro/intro__input.cmo

INTRO_CMXS = $(INTRO_CMOS:.cmo=.cmx)

INTRO_CMOS_SUBS = $(filter lib/intro/intro__%,$(INTRO_CMOS))
INTRO_CMXS_SUBS = $(INTRO_CMOS_SUBS:.cmo=.cmx)

PROP_LOGIC_CMOS =
PROP_LOGIC_CMOS += lib/prop_logic/prop_logic.cmo
PROP_LOGIC_CMOS += lib/prop_logic/prop_logic__syntax.cmo
PROP_LOGIC_CMOS += lib/prop_logic/prop_logic__semantics.cmo
PROP_LOGIC_CMOS += lib/prop_logic/prop_logic__parser.cmo
PROP_LOGIC_CMOS += lib/prop_logic/prop_logic__lexer.cmo
PROP_LOGIC_CMOS += lib/prop_logic/prop_logic__input.cmo

PROP_LOGIC_CMXS = $(PROP_LOGIC_CMOS:.cmo=.cmx)

PROP_LOGIC_CMOS_SUBS = $(filter lib/prop_logic/prop_logic__%,$(PROP_LOGIC_CMOS))
PROP_LOGIC_CMXS_SUBS = $(PROP_LOGIC_CMOS_SUBS:.cmo=.cmx)

ARCHIVES =
ARCHIVES += lib/intro/intro.cma
ARCHIVES += lib/prop_logic/prop_logic.cma

ARCHIVES_OPT =
ARCHIVES_OPT += lib/intro/intro.cmxa
ARCHIVES_OPT += lib/prop_logic/prop_logic.cmxa

TESTS =
TESTS += test/test_intro.byte
TESTS += test/test_prop_logic.byte

TESTS_OPT =
TESTS_OPT += test/test_intro.exe
TESTS_OPT += test/test_prop_logic.exe

TESTS_CRAM =
TESTS_CRAM += test/intro.t
TESTS_CRAM += test/prop_logic.t

-include config.mk

ifeq ($(HAS_CRAM),true)
TESTS += bin/main.byte
endif

.PHONY: all
all: byte opt

.PHONY: byte
byte: $(ARCHIVES) $(TESTS) bin/main.byte

.PHONY: opt
opt: $(ARCHIVES_OPT) $(TESTS_OPT) bin/main.exe

.PHONY: check
check: $(TESTS)
	./test/test_intro.byte
	./test/test_prop_logic.byte
ifeq ($(HAS_CRAM),true)
	$(PYTHON3) -m cram $(TESTS_CRAM)
endif

# intro

lib/intro/intro.cma: lib/intro/intro.cmi $(INTRO_CMOS)
	$(OCAMLC) -a $(INTRO_CMOS) -o $@

lib/intro/intro.cmxa: lib/intro/intro.cmi $(INTRO_CMXS)
	$(OCAMLOPT) -a $(INTRO_CMXS) -o $@

lib/intro/intro.cmi: lib/intro/intro.mli
	$(OCAMLC) $(OCAMLCFLAGS) -no-alias-deps -w -49 -c $<

lib/intro/intro.cmo: lib/intro/intro.ml
	$(OCAMLC) $(OCAMLCFLAGS) -no-alias-deps -w -49 -c $<

lib/intro/intro.cmx: lib/intro/intro.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -no-alias-deps -w -49 -c $<

$(INTRO_CMOS): INCLUDES += -I lib/intro
$(INTRO_CMXS): INCLUDES += -I lib/intro

$(INTRO_CMOS_SUBS): OCAMLCFLAGS += -no-alias-deps -open Intro
$(INTRO_CMXS_SUBS): OCAMLCFLAGS += -no-alias-deps -open Intro # for .mli -> .cmi
$(INTRO_CMXS_SUBS): OCAMLOPTFLAGS += -no-alias-deps -open Intro

lib/intro/intro__lexer.ml: lib/intro/intro__parser.mly

# prop_logic

lib/prop_logic/prop_logic.cma: lib/prop_logic/prop_logic.cmi $(PROP_LOGIC_CMOS)
	$(OCAMLFIND) $(OCAMLC) -a $(PROP_LOGIC_CMOS) -o $@

lib/prop_logic/prop_logic.cmxa: lib/prop_logic/prop_logic.cmi $(PROP_LOGIC_CMXS)
	$(OCAMLFIND) $(OCAMLOPT) -a $(PROP_LOGIC_CMXS) -o $@

lib/prop_logic/prop_logic.cmi: lib/prop_logic/prop_logic.mli
	$(OCAMLC) $(OCAMLCFLAGS) -no-alias-deps -w -49 -c $<

lib/prop_logic/prop_logic.cmo: lib/prop_logic/prop_logic.ml
	$(OCAMLC) $(OCAMLCFLAGS) -no-alias-deps -w -49 -c $<

lib/prop_logic/prop_logic.cmx: lib/prop_logic/prop_logic.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -no-alias-deps -w -49 -c $<

$(PROP_LOGIC_CMOS): INCLUDES += -I lib/prop_logic
$(PROP_LOGIC_CMXS): INCLUDES += -I lib/prop_logic

$(PROP_LOGIC_CMOS_SUBS): OCAMLCFLAGS += -no-alias-deps -open Prop_logic
$(PROP_LOGIC_CMXS_SUBS): OCAMLCFLAGS += -no-alias-deps -open Prop_logic # for .mli -> .cmi
$(PROP_LOGIC_CMXS_SUBS): OCAMLOPTFLAGS += -no-alias-deps -open Prop_logic

lib/prop_logic/prop_logic__lexer.ml: lib/prop_logic/prop_logic__parser.mly

lib/prop_logic/prop_logic__semantics.cmo: OCAMLFINDFLAGS += -linkpkg -package zarith
lib/prop_logic/prop_logic__semantics.cmx: OCAMLFINDFLAGS += -linkpkg -package zarith

# main

bin/main.%: OCAMLFINDFLAGS += -linkpkg -package zarith
bin/main.%: INCLUDES += -I lib/intro -I lib/prop_logic

bin/main.byte: lib/intro/intro.cma lib/prop_logic/prop_logic.cma bin/main.ml
	$(OCAMLFIND) $(OCAMLC) $(OCAMLCFLAGS) -o $@ $(OCAMLFINDFLAGS) $^

bin/main.exe: lib/intro/intro.cmxa lib/prop_logic/prop_logic.cmxa bin/main.ml
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ $(OCAMLFINDFLAGS) $^

# tests

test/test_intro.%: export OCAMLFIND_IGNORE_DUPS_IN = $(OCAMLC_WHERE)
test/test_intro.%: OCAMLFINDFLAGS += -linkpkg -package alcotest
test/test_intro.%: INCLUDES += -I lib/intro

test/test_intro.byte: lib/intro/intro.cma test/test_intro.ml
	$(OCAMLFIND) $(OCAMLC) $(OCAMLCFLAGS) -o $@ $(OCAMLFINDFLAGS) $^

test/test_intro.exe: lib/intro/intro.cmxa test/test_intro.ml
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ $(OCAMLFINDFLAGS) $^

test/test_prop_logic.%: export OCAMLFIND_IGNORE_DUPS_IN = $(OCAMLC_WHERE)
test/test_prop_logic.%: OCAMLFINDFLAGS += -linkpkg -package alcotest -package zarith
test/test_prop_logic.%: INCLUDES += -I lib/prop_logic

test/test_prop_logic.byte: lib/prop_logic/prop_logic.cma test/test_prop_logic.ml
	$(OCAMLFIND) $(OCAMLC) $(OCAMLCFLAGS) -o $@ $(OCAMLFINDFLAGS) $^

test/test_prop_logic.exe: lib/prop_logic/prop_logic.cmxa test/test_prop_logic.ml
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ $(OCAMLFINDFLAGS) $^

# general

%.ml: %.mll
	$(OCAMLLEX) $<

%.ml %.mli: %.mly
	$(MENHIR) --explain $<

%.cmi: %.mli
	$(OCAMLFIND) $(OCAMLC) $(OCAMLCFLAGS) $(OCAMLFINDFLAGS) -c $<

%.cmo: %.ml
	$(OCAMLFIND) $(OCAMLC) $(OCAMLCFLAGS) $(OCAMLFINDFLAGS) -c $<

%.cmx: %.ml
	$(OCAMLFIND) $(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLFINDFLAGS) -c $<

$(DESTDIR)$(bindir):
	mkdir -p $@

.PHONY: install
install: bin/main.byte bin/main.exe | $(DESTDIR)$(bindir)
	$(INSTALL_PROGRAM) bin/main.byte $(DESTDIR)$(bindir)/main.byte
	$(INSTALL_PROGRAM) bin/main.exe $(DESTDIR)$(bindir)/main.exe

.PHONY: clean
clean:
	rm -f $(INTRO_CMOS) $(INTRO_CMOS:.cmo=.cmt)
	rm -f $(INTRO_CMOS:.cmo=.cmi) $(INTRO_CMOS:.cmo=.cmti)
	rm -f $(INTRO_CMXS) $(INTRO_CMXS:.cmx=.o)
	rm -f $(PROP_LOGIC_CMOS) $(PROP_LOGIC_CMOS:.cmo=.cmt)
	rm -f $(PROP_LOGIC_CMOS:.cmo=.cmi) $(PROP_LOGIC_CMOS:.cmo=.cmti)
	rm -f $(PROP_LOGIC_CMXS) $(PROP_LOGIC_CMXS:.cmx=.o)
	rm -f $(ARCHIVES) $(ARCHIVES_OPT)
	rm -f bin/main.byte bin/main.exe
	rm -f $(TESTS) $(TESTS_OPT)

.PHONY: distclean
distclean: clean
	rm -f .depend
	rm -f $(GENERATED)
	rm -f config.mk

.depend: GNUmakefile $(GENERATED)
	@printf "# -*- mode: makefile; -*-\n\n" > $@
	@printf "# intro\n" >> $@
	$(OCAMLDEP) -I lib/intro -map lib/intro/intro.mli -open Intro $(INTRO_CMOS_SUBS:.cmo=.ml) >> $@
	@printf "\n# prop_logic\n" >> $@
	$(OCAMLDEP) -I lib/prop_logic -map lib/prop_logic/prop_logic.mli -open Prop_logic $(PROP_LOGIC_CMOS_SUBS:.cmo=.ml) >> $@

include .depend
