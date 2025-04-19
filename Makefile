MAKEFLAGS += --always-make

all:
	make --print-targets

desktop_config:
	$(MAKE) -C .src/desktop_config
