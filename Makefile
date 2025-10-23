ERL?=$(shell erl -noshell -eval 'io:format("~s", [code:root_dir()]).' -s init stop)
ERLANG_INCLUDE_DIR?=$(ERL)/usr/include
ERLANG_LIB_DIR?=$(ERL)/usr/lib
REBAR?=./rebar3
REBAR_CMD?=$(REBAR) compile

all: $(REBAR)
	$(REBAR_CMD)

$(REBAR):
	@echo "rebar3 not available; please install it or vendor it in repo"
	@exit 1

priv/iconverl.so:
	$(REBAR_CMD)

clean:
	$(REBAR) clean
