## $Id: Makefile,v 1.1 2005/02/13 09:45:06 vances Exp $

EBIN			= ../ebin/itu

ERL			= erl
ERLC			= erlc
EMULATOR		= beam
ERLCFLAGS	= -b $(EMULATOR) -o $(EBIN) -W -v +warn_unused_vars 

$(EBIN)/%.$(EMULATOR):%.erl
	$(ERLC) $(ERLCFLAGS) $<

BEAMS		=	$(EBIN)/map_dsm_fsm.$(EMULATOR) \
			$(EBIN)/map_load_fsm.$(EMULATOR)

.PHONY:	default
default:	all

.PHONY:	all
all:	$(BEAMS) gsm ansi

.PHONY:gsm 	
gsm:
	cd gsm && $(MAKE)

.PHONY:	ansi
ansi:
	cd ansi && $(MAKE)

.PHONY:	clean
clean:
	- rm -f $(BEAMS)
	cd gsm && $(MAKE) $@
	cd ansi && $(MAKE) $@

