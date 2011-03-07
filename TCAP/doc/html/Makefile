## $Id: Makefile,v 1.3 2005/02/10 06:39:56 vances Exp $

ERL		= erl

SRC		= ../../src

%.html:${SRC}/%.erl
	${ERL} -noshell -run edoc_run file '"$<"' '[{dir,"."}]' -s init stop

%.html:${SRC}/itu/%.erl
	${ERL} -noshell -run edoc_run file '"$<"' '[{dir,"."}]' -s init stop

DOCS		= tcap.html tcap_tco_server.html

.PHONY:	default
default:	doc

.PHONY:	doc
doc:	$(DOCS)

.PHONY:	clean
clean:
	- rm -f $(DOCS)

