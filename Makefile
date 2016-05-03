CAMLC = ocamlc
OCAMLCFLAGS= -c

SRCDIR   = src
OBJDIR   = _build
BINDIR   = test
TARGET   = IPRF_Analyseur_De_Texte_Romain_EPIARD

EXEC = $(BINDIR)/$(TARGET)

all: $(EXEC)

$(EXEC): $(OBJDIR)/questions.cmo $(OBJDIR)/textproc.cmo
	$(CAMLC) -o $(EXEC) $(OBJDIR)/questions.cmo $(OBJDIR)/textproc.cmo

$(OBJDIR)/questions.cmo: $(OBJDIR)/questions.cmi
	$(CAMLC) $(OCAMLCFLAGS) $(SRCDIR)/questions.ml

$(OBJDIR)/questions.cmi: $(SRCDIR)/questions.mli $(SRCDIR)/questions.ml
	$(CAMLC) $(OCAMLCFLAGS) $(SRCDIR)/questions.mli

$(OBJDIR)/textproc.cmo: $(OBJDIR)/textproc.cmi
	$(CAMLC) $(OCAMLCFLAGS) $(SRCDIR)/textproc.ml

$(OBJDIR)/textproc.cmi: $(SRCDIR)/textproc.mli $(SRCDIR)/textproc.ml
	$(CAMLC) $(OCAMLCFLAGS) $(SRCDIR)/textproc.mli

clean:
	rm -rf $(EXEC) $(OBJDIR)/*.cmi $(OBJDIR)/*.cmo
