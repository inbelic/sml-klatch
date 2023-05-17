GHC=ghc
GHCI=ghci

ODIR=../build

EXT_FLAGS=-XOverloadedStrings

build:
	cd Klatchlings; \
	$(GHC) --make -o $(ODIR)/klatch-game Main -odir $(ODIR) -hidir $(ODIR)

run: build
	cd build; \
	./klatch-game

clean:
	rm -rf build/*

.PHONY: build clean
