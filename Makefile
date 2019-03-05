bindir = $(HOME)/bin
libdir = $(HOME)/.local/lib
sbcl = sbcl
src = gpg-utilities.asd common.lisp \
	gpg-tofu.lisp gpg-graph.lisp gpg-cert-path.lisp
symlinks = gpg-tofu gpg-graph gpg-cert-path
conf = config.mk


-include $(conf)

all: gpg-utilities $(symlinks)

gpg-utilities: $(src)
	$(sbcl) --script make-image.lisp

$(symlinks): gpg-utilities
	ln -s gpg-utilities $@

config: $(conf)

$(conf):
	@echo "sbcl = $(sbcl)" > $@
	@echo "bindir = $(bindir)" >> $@
	@echo "libdir = $(libdir)" >> $@
	@echo --- $@
	@cat $@
	@echo ---

install:
	install -d -m 755 $(bindir)
	install -d -m 755 $(libdir)
	install -m 755 gpg-utilities $(libdir)
	cd $(bindir) && { \
		ln -fs $(libdir)/gpg-utilities gpg-tofu; \
		ln -fs $(libdir)/gpg-utilities gpg-graph; \
		ln -fs $(libdir)/gpg-utilities gpg-cert-path; \
		}

uninstall:
	rm -f $(bindir)/gpg-tofu
	rm -f $(bindir)/gpg-graph
	rm -f $(bindir)/gpg-cert-path
	rm -f $(libdir)/gpg-utilities

clean:
	rm -f -- gpg-utilities $(symlinks) *.fasl

clean-all: clean
	rm -f $(conf)

.PHONY: all config install uninstall clean clean-all
