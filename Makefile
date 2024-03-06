# Makefile --- Puppet Treesitter Mode for Emacs

CASK       = cask
EMACS      = emacs
EMACSFLAGS =
TESTFLAGS  =

SRCS       = puppet-ts-mode.el
OBJS       = $(SRCS:.el=.elc)

export EMACS

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)


.PHONY: compile test clean

compile: $(OBJS)

clean:
	rm -f $(OBJS)

test: $(PKGDIR)
	$(CASK) exec ert-runner $(TESTFLAGS)

%.elc: %.el $(PKGDIR)
	$(CASK) exec $(EMACS) -Q --batch $(EMACSFLAGS) -f batch-byte-compile $<

$(PKGDIR): Cask
	$(CASK) install
	touch $(PKGDIR)
