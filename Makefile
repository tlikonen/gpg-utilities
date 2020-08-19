prefix = /usr/local
bindir = $(prefix)/bin
libdir = $(prefix)/lib
sbcl = $(shell command -v sbcl)
gpg = gpg
src = src/*.asd src/*.lisp
prg = gpg-tofu gpg-graph gpg-cert-path gpg-count-steps

-include config.mk

gpg-utilities: $(patsubst %,build/%,$(prg))

usage: $(patsubst %,%.txt,$(prg))

all: gpg-utilities usage

$(patsubst %,build/%,$(prg)): $(src) quicklisp/setup.lisp
	$(sbcl) --script make.lisp $(patsubst build/%,%,$@) '$(gpg)' '$(libdir)/gpg-utilities/'

quicklisp/install.lisp:
	mkdir -p quicklisp
	wget -O $@ "http://beta.quicklisp.org/quicklisp.lisp"

quicklisp/setup.lisp: quicklisp/install.lisp
	$(sbcl) --noinform --no-sysinit --no-userinit --non-interactive \
		--load asdf.conf \
		--load quicklisp/install.lisp \
		--eval '(quicklisp-quickstart:install :path "quicklisp/")'

config.mk:
	@echo "bindir = $(bindir)" > $@
	@echo "libdir = $(libdir)" >> $@
	@echo "sbcl = $(sbcl)" >> $@
	@echo "gpg = $(gpg)" >> $@
	@cat $@

$(patsubst %,%.txt,$(prg)): $(src) quicklisp/setup.lisp
	$(sbcl) --script usage.lisp $@

install:
	install -d -m 755 "$(bindir)" "$(libdir)/gpg-utilities"
	install -m 755 build/gpg-tofu "$(bindir)"
	install -m 755 build/gpg-graph "$(bindir)"
	install -m 755 build/gpg-cert-path "$(bindir)"
	install -m 755 build/gpg-count-steps "$(bindir)"
	install -m 644 build/src/gpg-utilities.asd "$(libdir)/gpg-utilities"
	install -m 644 build/src/gpg-utilities--all-systems.fasl "$(libdir)/gpg-utilities"

uninstall:
	rm -f "$(bindir)/gpg-tofu"
	rm -f "$(bindir)/gpg-graph"
	rm -f "$(bindir)/gpg-cert-path"
	rm -f "$(bindir)/gpg-count-steps"
	rm -fr "$(libdir)/gpg-utilities"

clean:
	rm -fr build

distclean: clean
	rm -fr quicklisp
	rm -f config.mk

.PHONY: gpg-utilities usage all install uninstall clean distclean
