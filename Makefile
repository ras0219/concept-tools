all: cxx_concepts.png cxx_concepts_subset.png reduce_concepts doc

%.png: %.dot
	dot -Tpng -o$@ $<
%: %.hs
	ghc --make $<

HASKELLSOURCES := $(wildcard *.hs)

doc: $(HASKELLSOURCES)
	haddock $< --html

clean:
	rm -f reduce_concepts *.o *.hi
