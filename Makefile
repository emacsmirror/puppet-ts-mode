# Makefile --- Puppet Treesitter Mode for Emacs

CASK    := cask
EMACS   := emacs

SRCS    := $(shell cask files)
OBJS    := $(SRCS:.el=.elc)
CHECKS  := $(wildcard test/*-test.el)
PKGDIR  := $(shell $(CASK) package-directory)

#
# rules
#

all: compile check

compile: $(OBJS)

check: compile $(CHECKS) clean

clean:
	@$(CASK) clean-elc

%.elc: %.el $(PKGDIR)
	@$(CASK) build

test/%.el: .FORCE
	@$(CASK) emacs -Q -batch -L . -L test \
		-l ert -l test/test-helper -l $@ \
		-f ert-run-tests-batch-and-exit

$(PKGDIR): Cask
	@$(CASK) install
	@touch $(PKGDIR)

#
# special targets
#

.FORCE:
.PHONY: all compile check clean
