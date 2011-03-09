## $Id: Makefile,v 1.2 2005/02/13 00:42:12 vances Exp $

ERL		= erl

SRC		= ../../src

DOCS		= map.html

%.html:${SRC}/%.erl
	${ERL} -noshell -run edoc_run file '"$<"' '[{dir,"."}]' -s init stop

%.html:${SRC}/ansi/%.erl
	${ERL} -noshell -run edoc_run file '"$<"' '[{dir,"."}]' -s init stop

%.html:${SRC}/gsm/%.erl
	${ERL} -noshell -run edoc_run file '"$<"' '[{dir,"."}]' -s init stop


.PHONY:	all
all:	doc
	

.PHONY:	doc
doc:	${DOCS}

.PHONY:	clean
clean:
	- rm ${DOCS}

tcap.html:	${ESRC}/tcap.erl

