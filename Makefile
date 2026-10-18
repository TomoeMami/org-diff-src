EMACS ?= emacs
PACKAGE = org-diff-src.el
TEST = test/org-diff-src-test.el

.PHONY: all compile test checkdoc package-lint clean

all: compile

compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $(PACKAGE)

test:
	$(EMACS) -Q --batch -L . -l $(TEST) -f ert-run-tests-batch-and-exit

checkdoc:
	$(EMACS) -Q --batch -L . --eval '(progn (require (quote checkdoc)) (find-file "$(PACKAGE)") (checkdoc-current-buffer t) (princ "checkdoc: OK\n"))'

package-lint:
	$(EMACS) -Q --batch -L . --eval '(progn (require (quote package-lint)) (find-file "$(PACKAGE)") (let ((issues (package-lint-buffer))) (if issues (progn (dolist (issue issues) (princ (format "%S\n" issue))) (kill-emacs 1)) (princ "package-lint: OK\n"))))'

clean:
	rm -f *.elc test/*.elc
