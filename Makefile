OCAMLC = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
OCAMLDEP = ocamlfind ocamldep

OCAMLCFLAGS = -I ptrees
OCAMLOPTFLAGS = -I ptrees

PACK_OFSM_DEP = ptrees/ptset ptrees/ptmap \
  sigUtil hFSM ptFSM traversal basic ops det min output full
PACK_OFSM_DEPI = sig
PACK_OFSM = OFSM
PACK_OFSM_FILE = $(shell echo $(PACK_OFSM) | sed 's/^./\l\0/') # Lower first letter

LIB_OFSM_DEP = $(PACK_OFSM_FILE)
LIB_OFSM = oFSM

all: $(LIB_OFSM).cma $(LIB_OFSM).cmxa

# Pack #########################################################################

$(addsuffix .cmo,$(PACK_OFSM_FILE)) : $(addsuffix .cmo,$(PACK_OFSM_DEP)) $(addsuffix .cmi,$(PACK_OFSM_DEPI))
	$(OCAMLC) $(OCAMLCFLAGS) -pack $^ -o $@

$(addsuffix .cmx,$(PACK_OFSM_FILE)) : $(addsuffix .cmx,$(PACK_OFSM_DEP)) $(addsuffix .cmi,$(PACK_OFSM_DEPI))
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -pack $^ -o $@

$(addsuffix .cmx,$(PACK_OFSM_DEP)): OCAMLOPTFLAGS+=-for-pack $(PACK_OFSM)

# Library ######################################################################

$(LIB_OFSM).cma : $(addsuffix .cmo,$(LIB_OFSM_DEP))
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ -a $^

$(LIB_OFSM).cmxa : $(addsuffix .cmx,$(LIB_OFSM_DEP))
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ -a $^

test: oFSM.cma test.cmo
	$(OCAMLC) $(OCAMLCFLAGS) unix.cma $^ -o $@

# Generic ######################################################################

%.cmo:%.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c $< 

%.cmx:%.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

%.cmi:%.mli
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

# Dependencies #################################################################

include .depend

.depend depend:
	$(OCAMLDEP) `find -name '*.ml' -o -name '*.mli'` > .depend

# Clean ########################################################################

distclean clean:
	$(RM) *.cmo *.cmx *.cmi *.*/cmx */*.cmo */*.cmi

