ERL?=$(shell erl -noshell -eval 'io:format("~s", [code:root_dir()]).' -s init stop)
ERLANG_INCLUDE_DIR?=$(ERL)/usr/include
ERLANG_LIB_DIR?=$(ERL)/usr/lib
REBAR?=./rebar3
REBAR_URL?=https://github.com/erlang/rebar3/releases/latest/download/rebar3
REBAR_CMD?=$(REBAR) compile

all: priv/iconverl.so

$(REBAR):
	@echo "Fetching rebar3..."
	@if command -v curl >/dev/null 2>&1; then \
		curl -fsSL -o rebar3 $(REBAR_URL); \
	else \
		wget -qO rebar3 $(REBAR_URL); \
	fi
	@chmod +x rebar3

priv/iconverl.so: $(REBAR) c_src/iconverl.c src/iconverl.erl rebar.config
	$(REBAR_CMD)

clean:
	$(REBAR) clean
