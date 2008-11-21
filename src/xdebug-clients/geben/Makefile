#!/usr/bin/make
#
# Makefile for GEBEN

EMACS   = emacs
CP      = cp -p
RM      = rm -f
INSTALL = install

.el.elc:
	$(EMACS) -Q --batch --eval '(byte-compile-file "$<")'

SRCS    = geben.el
OBJS    = $(SRCS:%.el=%.elc)
IMGDIR  = tree-widget/geben
IMGS    = $(wildcard $(IMGDIR)/*.png)

GUESS-SITELISP := $(shell $(EMACS) -Q --batch --eval '	       \
  (let (tbl)						       \
    (mapc (lambda (path)				       \
	    (if (string-match "^\\(.*/site-lisp\\)\\b/?" path) \
		(let* ((spath (match-string 1 path))	       \
		       (pair (assoc spath tbl)))	       \
		  (if pair				       \
		      (setcdr pair (1+ (cdr pair)))	       \
		    (setq tbl (cons (cons spath 1) tbl))))))   \
	  load-path)					       \
    (princ (or (car (car (sort tbl (lambda (a b)	       \
				     (> (cdr a) (cdr b))))))   \
	       "")))')

ifndef SITELISP
SITELISP := $(GUESS-SITELISP)
ifeq ($(SITELISP), nil)
$(error Cannot find appropriate site-lisp directory.)
endif
endif


DEST = $(SITELISP)/geben
DEST-IMG = $(DEST)/tree-widget/geben

.PHONY: all
all: $(OBJS)

.PHONY: install
install: all
	$(INSTALL) -m 755 -d $(DEST)
	$(INSTALL) -m 644 $(SRCS) $(OBJS) $(DEST)
	$(INSTALL) -m 755 -d $(DEST-IMG)
	$(INSTALL) -m 644 $(IMGS) $(DEST-IMG)

.PHONY: clean
clean:
	$(RM) $(OBJS)
