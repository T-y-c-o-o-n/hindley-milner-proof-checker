SOURCES=src/*
PACKAGE=hindley_milner_proof_checker.zip

.PHONY: pack all run clean

all: proofchecker

run: proofchecker
	./proofchecker

clean:
	rm -f proofchecker

proofchecker: $(SOURCES)
	ghc -i./src -tmpdir . ./app/Main.hs -o proofchecker -package megaparsec -package containers

pack:
	zip $(PACKAGE) -r Makefile src app