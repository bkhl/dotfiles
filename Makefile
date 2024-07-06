.PHONY: all desktop_config

all:
	make --print-targets

desktop_config:
	$(MAKE) -C .src/desktop_config

README.html: README.org
	emacs --quick --batch --load org $< --eval "(org-html-export-to-html nil nil nil t)"
