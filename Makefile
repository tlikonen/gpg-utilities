bindir = $(HOME)/bin
sbcl = sbcl
gpg = gpg
src = src/*.asd src/*.lisp
links = gpg-tofu gpg-graph gpg-cert-path gpg-count-steps
conf = config.mk

-include $(conf)

all: $(links)

gpg-utilities: $(src)
	$(sbcl) --script make-image.lisp '$(gpg)'

$(links): gpg-utilities
	ln -f gpg-utilities $@

config: $(conf)

$(conf):
	@echo "sbcl = $(sbcl)" > $@
	@echo "gpg = $(gpg)" >> $@
	@echo "bindir = $(bindir)" >> $@
	@echo --- $@
	@cat $@
	@echo ---

install:
	install -d -m 755 $(bindir)
	install -m 755 gpg-utilities $(bindir)
	cd $(bindir) && { \
		ln -f gpg-utilities gpg-tofu; \
		ln -f gpg-utilities gpg-graph; \
		ln -f gpg-utilities gpg-cert-path; \
		ln -f gpg-utilities gpg-count-steps; \
		}

uninstall:
	rm -f $(bindir)/gpg-tofu
	rm -f $(bindir)/gpg-graph
	rm -f $(bindir)/gpg-cert-path
	rm -f $(bindir)/gpg-count-steps
	rm -f $(bindir)/gpg-utilities

clean:
	rm -f -- gpg-utilities $(links) src/*.fasl

clean-all: clean
	rm -f $(conf)

.PHONY: all config install uninstall clean clean-all
