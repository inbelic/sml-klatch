CXX := erlc

ODIR = build
OUT = -o $(ODIR)

_OBJS= misc.beam \
	  client_router.beam client_sm.beam client_srvr.beam \
	  game_port.beam game_queue.beam game_relay.beam game_resource.beam \
	  harness.beam \
	  slatch_sup.beam game_sup.beam client_sup.beam routing_sup.beam \
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
