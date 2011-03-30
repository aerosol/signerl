
EBIN = ../../ebin
DOC = ../../doc/html

NMSINC = ../../../nms_erldrv/include

INCLUDES = $(NMSINC)

ERLC = erlc
ERL = erl
ERLCFLAGS = -W -v -o $(EBIN) -I $(INCLUDES)
DEBUGFLAGS = +debug_info

$(EBIN)/%.beam:%.erl
	${ERLC} $(ERLCFLAGS) $<

$(DOC)/%.html:%.erl
	${ERL} -noshell -run edoc_run file '"$<"' '[{dir, "$(DOC)"}]' -s init stop

BEAMS = $(EBIN)/sccp.beam

DOCS = $(DOC)/sccp.html

$(EBIN)/sccp.app:	$(EBIN)/sccp.app.src
	sed -e "s;%VERSION%;${VERSION};" $(EBIN)/sccp.app.src > $(EBIN)/sccp.app
	
all:	$(EBIN)/sccp.app beams docs

.PHONY:	beams
beams:	$(BEAMS)

.PHONY:	docs
docs:	$(DOCS)

.PHONY:	install
install:	all
	install -d $(ERL_TOP)/lib/sccp-${VERSION}/ebin
	install -f $(ERL_TOP)/lib/sccp-${VERSION}/ebin $(EBIN)/sccp.app
	install -f $(ERL_TOP)/lib/sccp-${VERSION}/ebin $(BEAMS)
	install -d $(ERL_TOP)/lib/sccp-${VERSION}/doc/html
	install -f $(ERL_TOP)/lib/sccp-${VERSION}/doc/html $(DOCS)
	
.PHONY:	clean
clean:
	rm -rf $(EBIN)/sccp.app
	rm -rf $(BEAMS)
	rm -rf $(DOCS)

sccp.erl:	$(NMSINC)/nms_sccp.hrl

