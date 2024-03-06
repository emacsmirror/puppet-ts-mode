# Makefile --- Puppet Treesitter Mode for Emacs

CASK    := cask
EMACS   := emacs

SRCS    := $(shell cask files)
OBJS    := $(SRCS:.el=.elc)
TESTS   := $(wildcard test/*-test.el)
PKGDIR  := $(shell $(CASK) package-directory)

#
# rules
#

all: compile tests

compile: $(OBJS)

tests: $(TESTS) | $(PKGDIR)

%.elc: %.el | $(PKGDIR)
	@$(CASK) $(EMACS) -Q -batch -f batch-byte-compile $<

test/%.el: FORCE
	@$(CASK) $(EMACS) -Q -batch -L . -L test \
		-l ert -l test/test-helper -l $@ \
		-f ert-run-tests-batch-and-exit

$(PKGDIR): Cask
	@$(CASK) install
	@touch $(PKGDIR)

#
# special targets
#

FORCE:
.PHONY: compile tests
