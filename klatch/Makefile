GHC=ghc
GHCI=ghci

ODIR=../build

EXT_FLAGS=-XOverloadedStrings
_PACKAGES=base containers bytestring network
PACKAGES=$(patsubst %,$\-package %,$(_PACKAGES))

build:
	cd Klatchlings; \
	$(GHC) --make -o $(ODIR)/klatch-game Main -odir $(ODIR) -hidir $(ODIR) $(PACKAGES)

run: build
	cd build; \
	./klatch-game

clean:
	rm -rf build/*

.PHONY: build clean
