
.PHONY: all build run test clean

all: SuccinctDS.pdf build

SuccinctDS.pdf: *.tex lib/*.lhs test/*.lhs exec/*.lhs references.bib
	latexmk -pdf -bibtex -synctex=1 -interaction=nonstopmode SuccinctDS

build:
	stack build

run:
	stack build && stack exec myprogram

test:
	stack test --coverage

clean:
	stack clean
	rm -f *.aux *.log *.out *.snm *.toc *.vrb *.nav *.synctex.gz *.blg *.bbl *.fdb_latexmk *.fls *.ind *.idx *.ilg *.bcf *.run.xml *.xdv
