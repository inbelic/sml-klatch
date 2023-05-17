CXX := erlc

ODIR = build
OUT = -o $(ODIR)

_OBJS=harness.beam client_mgr.beam game_port.beam \
	  game_sup.beam
OBJS=$(patsubst %,$(ODIR)/%,$(_OBJS))

SRC=src

NAME=server

build: $(NAME)

run: build
	cd build; \
	erl

$(ODIR)/%.beam: $(SRC)/%.erl
	$(CXX) $(OUT) $<

$(NAME): $(OBJS)

clean:
	rm -rf build/*
