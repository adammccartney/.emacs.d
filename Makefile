SHELL := /usr/bin/bash
EMACS   = emacs
VERSION = $$($(EMACS) -Q -batch --eval '(princ emacs-version)')

compile: init 
	$(EMACS) -Q -batch -l init.el -l compile.el

#magit: post-init-hook
#	$(MAKE) site-lisp/$(VERSION)/dash
#	$(MAKE) site-lisp/$(VERSION)/transient
#	$(MAKE) site-lisp/$(VERSION)/with-editor
#	$(MAKE) site-lisp/$(VERSION)/magit
#
#post-init-hook: init
#	$(SHELL) config.mk/magit.sh

init:
	$(EMACS) -Q -batch -L lisp -l packages.el
	cp -r lisp etc site-lisp/$(VERSION)

clean:
	rm -rf auto-save-list
	rm -rf site-lisp/*/lisp site-lisp/*/etc

mostlyclean:
	rm -rf auto-save-list
	rm -rf site-lisp

distclean:
	rm -rf auto-save-list
	rm -rf site-lisp
	rm -rf gpkg
