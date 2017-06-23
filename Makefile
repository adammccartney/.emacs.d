EMACS = emacs

init:
	$(EMACS) -Q -batch -L lisp -l packages.el -f gpkg-compile
	$(EMACS) -Q -batch -L lisp -L etc -l setup.el

clean:
	find lisp etc -name "*.elc" -exec rm {} \;
	rm -f init.elc
	rm -f -r auto-save-list/

mostlyclean: clean
	$(EMACS) -Q -batch -L lisp -l packages.el -f gpkg-clean

distclean: clean
	rm -rf gpkg
