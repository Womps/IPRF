CAMLC = ocamlc
OCAMLCFLAGS= -c

SRCDIR   = src
OBJDIR   = obj
BINDIR   = test
TARGET   = IPRF_Analyseur_De_Texte_Romain_EPIARD

EXEC = $(BINDIR)/$(TARGET)

all: $(EXEC)

$(EXEC): $(OBJDIR)/questions.cmo $(OBJDIR)/textproc.cmo
      $(CAMLC) -o $(EXEC) $(OBJDIR)/questions.cmo $(OBJDIR)/textproc.cmo

questions.cmo: $(OBJDIR)/questions.cmi
      $(CAMLC) $(OCAMLCFLAGS) $(SRCDIR)/questions.ml

questions.cmi: $(SRCDIR)/questions.mli $(SRCDIR)/questions.ml
      $(CAMLC) $(OCAMLCFLAGS) $(SRCDIR)/questions.mli

textproc.cmo: $(OBJDIR)/textproc.cmi
      $(CAMLC) $(OCAMLCFLAGS) $(SRCDIR)/textproc.ml

textproc.cmi: $(SRCDIR)/textproc.mli $(SRCDIR)/textproc.ml
	  $(CAMLC) $(OCAMLCFLAGS) $(SRCDIR)/textproc.mli

clean:
    rm -rf $(EXEC) $(OBJDIR)/*.cmi $(OBJDIR)/*.cmo