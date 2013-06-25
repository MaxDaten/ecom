# pdf creator and index creator
LATEX  = pdflatex
INDEX  = makeindex

# filename ant extensions
FILE = ecom
TEXEXT = .tex
PDFEXT = .pdf
IDXEXT = .idx
NOMEXT = .nlo
GLSEXT = .nls

#
VIEW = okular
GREP = grep
LS   = ls

all:
	@make compile
	@make index
	@make glossary
	@make compile

compile:
	@$(LATEX)  $(FILE)$(TEXEXT)

index:
	@$(INDEX) $(FILE)$(IDXEXT) -g -s index.ist

glossary:
	@$(INDEX) $(FILE)$(NOMEXT) -s nomencl.ist -o $(FILE)$(GLSEXT)

view:
	@$(VIEW) $(FILE)$(PDFEXT) &

clean:
	@$(LS) $(FILE).* | $(GREP) -v $(TEXEXT) | xargs $(RM)
	@$(RM) *~
	@$(RM) *.aux
	@$(RM) chapters/*.aux
