# Makefile --- Puppet Treesitter Mode for Emacs

CASK    := cask
EMACS   := emacs

SRCS    := $(filter-out %-autoloads.el, $(shell cask files))
OBJS    := $(SRCS:.el=.elc)
CHECKS  := $(wildcard test/*-test.el)
PKGDIR  := $(shell $(CASK) package-directory)

#
# rules
#

all: compile check lint clean

compile: $(OBJS)

check: compile $(CHECKS)

clean:
	@$(CASK) clean-elc
	@$(RM) *-autoloads.el

lint: lint-package lint-elisp

lint-package:
	@$(CASK) $(EMACS) --batch \
	--eval "(require 'package)" \
	--eval "(setq package-archives '((\"melpa\" . \"http://melpa.org/packages/\")))" \
	--eval "(package-initialize)" \
	--eval "(package-refresh-contents)" \
	-L '$(abspath .)' \
	-l package-lint -f package-lint-batch-and-exit $(SRCS)

lint-elisp:
	@$(CASK) $(EMACS) --batch \
	--eval "(require 'package)" \
	--eval "(setq package-archives '((\"melpa\" . \"http://melpa.org/packages/\")))" \
	--eval "(package-initialize)" \
	--eval "(package-refresh-contents)" \
	--eval "(setq make-backup-files nil)" \
	-L '$(abspath .)' \
	-l elisp-lint \
	-f elisp-lint-files-batch --no-indent-character --no-fill-column \
	$(SRCS)

%.elc: %.el $(PKGDIR)
	@$(CASK) build

test/%.el: .FORCE
	@$(CASK) $(EMACS) -Q -batch -L '$(abspath .)' -L test \
		-l ert -l test/test-helper -l $@ \
		-f ert-run-tests-batch-and-exit

$(PKGDIR): Cask
	@$(CASK) install
	@touch $(PKGDIR)

#
# special targets
#

.FORCE:
.PHONY: all compile check clean lint lint-package lint-elisp
