GHC=ghc

build:
	cd Klatchlings; \
	$(GHC) --make -o ../build/klatch-game Main -odir ../build -hidir ../build

run: build
	cd build; \
	./klatch-game

clean:
	rm -rf build/*

.PHONY: build clean
