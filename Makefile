all: cxx_concepts.png cxx_concepts_subset.png reduce_concepts

%.png: %.dot
	dot -Tpng -o$@ $<
%: %.hs
	ghc --make $<

clean:
	rm -f reduce_concepts *.o *.hi
