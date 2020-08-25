.POSIX:
EMACS = emacs

SRC = blorg.el
TESTSRC = blorg-tests.el

OBJ = blorg.elc
TESTOBJ = blorg-tests.elc
TEMPLATEL = "(add-to-list 'load-path \"~/src/github.com/clarete/templatel/\")"

ALLOBJS = $(OBJ)
#$(TESTOBJ)

all: $(ALLOBJS)

$(OBJ): $(SRC)
$(TESTOBJ): $(OBJ) $(TESTSRC)

clean:; rm -f $(ALLOBJS)

check: $(TESTOBJ)
	$(EMACS) -batch -Q -L . --eval $(TEMPLATEL) -l $(TESTOBJ) -f ert-run-tests-batch-and-exit

check-one: $(TESTOBJ)
	$(EMACS) -batch -Q -L . --eval $(TEMPLATEL) -l $(TESTOBJ) --eval "(ert-run-tests-batch-and-exit '(member ${TEST}))"

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -batch -Q -L . --eval $(TEMPLATEL) -f batch-byte-compile $<
