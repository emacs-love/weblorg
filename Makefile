.POSIX:
EMACS ?= cask exec emacs

SRC = blorg.el
TESTSRC = t/blorg-tests.el

OBJ = blorg.elc
TESTOBJ = t/blorg-tests.elc
ALLOBJS = $(OBJ) $(TESTOBJ)

# NOTE: This is meant to accelerate local development.  Although
# templatel is being installed via Cask, this generic path is still
# being injected into `load-path' so if there's a copy of templatel
# there, it will be prioritized over the one installed by Cask.
TEMPLATEL = --eval "(add-to-list 'load-path \"~/src/github.com/clarete/templatel/\")"

CASK_MARK = .cask/done

all: $(ALLOBJS)

$(OBJ): $(SRC)
$(TESTOBJ): $(OBJ) $(TESTSRC)

clean:; rm -f $(ALLOBJS)

$(CASK_MARK):; cask install && touch .cask/done
deps: $(CASK_MARK)

check: deps $(TESTOBJ)
	$(EMACS) -batch -Q -L . $(TEMPLATEL) -l $(TESTOBJ) -f ert-run-tests-batch-and-exit

check-one: $(TESTOBJ)
	$(EMACS) -batch -Q -L . $(TEMPLATEL) -l $(TESTOBJ) --eval "(ert-run-tests-batch-and-exit '(member ${TEST}))"

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -batch -Q -L . $(TEMPLATEL) -f batch-byte-compile $<
