CXX := erlc

ODIR = build
OUT = -o $(ODIR)

_OBJS=str_conv.beam game_resource.beam game_queue.beam \
	  harness.beam client_mgr.beam game_port.beam client_srvr.beam \
	  slatch_sup.beam game_sup.beam client_sup.beam \
	  slatch_app.beam
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
