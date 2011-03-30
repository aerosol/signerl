## $Id: Makefile,v 1.5 2005/02/10 05:20:17 vances Exp $

EBIN			= ../ebin

ERL			= erl
ERLC			= erlc
EMULATOR		= beam
ERLCFLAGS	= -b $(EMULATOR) -o $(EBIN) -W -v +warn_unused_vars 

$(EBIN)/%.$(EMULATOR):%.erl
	$(ERLC) $(ERLCFLAGS) $<

BEAMS		=	$(EBIN)/tcap.$(EMULATOR) $(EBIN)/tcap_app.$(EMULATOR) \
						$(EBIN)/tcap_sup.$(EMULATOR) \
						$(EBIN)/tcap_sap_sup.$(EMULATOR) \
						$(EBIN)/tcap_transaction_sup.$(EMULATOR) \
						$(EBIN)/tcap_dialogue_sup.$(EMULATOR) \
						$(EBIN)/tcap_components_sup.$(EMULATOR) \
						$(EBIN)/tcap_invocation_sup.$(EMULATOR)

.PHONY:	default
default:	all

.PHONY:	all
all:	$(BEAMS) itu ansi

.PHONY:	itu
itu:
	cd ITU && $(MAKE)

.PHONY:	ansi
ansi:
	cd ANSI && $(MAKE)

.PHONY:	clean
clean:
	- rm -f $(BEAMS)
	cd ITU && $(MAKE) $@
	cd ANSI && $(MAKE) $@

