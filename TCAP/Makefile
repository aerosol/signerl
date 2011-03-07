
.PHONY:	all
all:
	cd asn_src && $(MAKE)
	cd src && $(MAKE)
	cd doc && $(MAKE)

.PHONY:	doc
doc:
	cd doc && $(MAKE) $@

.PHONY:	clean
clean:
	cd asn_src && $(MAKE) $@
	cd src && $(MAKE) $@
	cd doc && $(MAKE) $@

