CAMLC =ocamlc

SRCDIR   = src
OBJDIR   = _build
BINDIR   = test
TARGET   = IPRF_Analyseur_De_Texte_Romain_EPIARD

OCAMLCFLAGS=-c -I $(OBJDIR)
EXEC = $(BINDIR)/$(TARGET)

all: $(EXEC)

$(EXEC): $(OBJDIR)/questions.cmo
	$(CAMLC) -o $(EXEC) $(OBJDIR)/questions.cmo

$(OBJDIR)/questions.cmo: $(OBJDIR)/questions.cmi
	$(CAMLC) $(OCAMLCFLAGS) $(SRCDIR)/questions.ml
	mv $(SRCDIR)/questions.cmo $(OBJDIR)/questions.cmo

$(OBJDIR)/questions.cmi: $(SRCDIR)/questions.mli $(SRCDIR)/questions.ml
	$(CAMLC) $(OCAMLCFLAGS) $(SRCDIR)/questions.mli
	mv $(SRCDIR)/questions.cmi $(OBJDIR)/questions.cmi

clean:
	rm -rf $(EXEC) $(OBJDIR)/*.cmi $(OBJDIR)/*.cmo
