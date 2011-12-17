## $id$

SCCPINCDIR = ../../../SCCP/itu/include

EBIN			= ../../ebin/itu

ERLC			= erlc
EMULATOR		= beam
ERLCFLAGS	= -b $(EMULATOR) -o $(EBIN) -W -v +warn_unused_vars -I ../../asn_src -I ../../include -I $(SCCPINCDIR)

$(EBIN)/%.$(EMULATOR):%.erl
	$(ERLC) $(ERLCFLAGS) $<

BEAMS		=	$(EBIN)/tcap_tco_server.$(EMULATOR) \
						$(EBIN)/tcap_tsm_fsm.$(EMULATOR) \
						$(EBIN)/tcap_dha_fsm.$(EMULATOR) \
						$(EBIN)/tcap_cco_server.$(EMULATOR) \
						$(EBIN)/tcap_ism_fsm.$(EMULATOR) 

.PHONY:	default
default:	all

.PHONY:	all
all:	$(BEAMS)

.PHONY:	install

.PHONY:	clean
clean:
	- rm -f $(BEAMS)

tcap_tco_server.$(EMULATOR):	tcap_tco_server.erl \
				../../asn_src/TCAPMessages.hrl ../../include/tcap.hrl \
				$(SCCPINCDIR)/sccp.hrl
tcap_tsm_fsm.$(EMULATOR):	tcap_tsm_fsm.erl \
				../../asn_src/TCAPMessages.hrl ../../include/tcap.hrl \
				$(SCCPINCDIR)/sccp.hrl

