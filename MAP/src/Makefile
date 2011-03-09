## $Id: Makefile,v 1.1 2005/02/13 00:42:12 vances Exp $

EBIN			= ../ebin

ERL			= erl
ERLC			= erlc
EMULATOR		= beam
ERLCFLAGS	= -b $(EMULATOR) -o $(EBIN) -W -v +warn_unused_vars 

$(EBIN)/%.$(EMULATOR):%.erl
	$(ERLC) $(ERLCFLAGS) $<

BEAMS		=	$(EBIN)/map.$(EMULATOR) $(EBIN)/map_app.$(EMULATOR) \
						$(EBIN)/map_sup.$(EMULATOR) \
						$(EBIN)/map_pm_sup.$(EMULATOR)

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

