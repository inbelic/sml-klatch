SUBDIRS_BUILD := klatch.build slatch.build
SUBDIRS_CLEAN := klatch.clean slatch.clean

build: $(SUBDIRS_BUILD)

run: build
	$(MAKE) run -C slatch

clean: $(SUBDIRS_CLEAN)

%.build:
	$(MAKE) build -C $*

%.clean:
	$(MAKE) clean -C $*
