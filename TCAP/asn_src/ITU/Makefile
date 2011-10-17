EBIN			= ../../ebin/itu

ERLC			= erlc
ERLCFLAGS	= -b beam -W -v +warn_unused_vars -o $(EBIN)

ASNC			= $(ERLC) 
ASNCFLAGS	= -W -b ber_bin +optimize +driver

$(EBIN)/%.beam:%.erl
	$(ERLC) $(ERLCFLAGS) $<

.SECONDARY:	%.erl
%.erl:%.asn
	$(ASNC) $(ASNCFLAGS) $<

.PHONY:	default
default:	all

.PHONY:	all
all:		q773 q755 q775 simple

.PHONY:	q773
q773:		$(EBIN)/TR.beam $(EBIN)/TCAPMessages.beam \
			$(EBIN)/DialoguePDUs.beam $(EBIN)/UnidialoguePDUs.beam

TR.erl:	TCAPMessages.erl
TCAPMessages.erl:	Remote-Operations-Useful-Definitions.erl

.PHONY:	q755
q755:		$(EBIN)/TC-Testing-User.beam

TC-Testing-User.erl:		TC-TMP.erl Remote-Operations-Information-Objects.erl

.PHONY:	q775
q775:		$(EBIN)/TCAP-Examples.beam $(EBIN)/TCAP-Tools.beam 

.PHONY: simple
simple: 	$(EBIN)/SimpleTCAP.beam

TCAP-Examples.erl:	Remote-Operations-Information-Objects.erl
TCAP-Tools.erl:		Remote-Operations-Information-Objects.erl
TC-Notation-Extensions.erl:	TCAPMessages.erl \
		Remote-Operations-Useful-Definitions.erl \
		Remote-Operations-Information-Objects.erl \
		Remote-Operations-Generic-ROS-PDUs.erl \
		UnidialoguePDUs.erl DialoguePDUs.erl

Remote-Operations-Useful-Definitions.erl:	\
			Remote-Operations-Information-Objects.asn \
			Remote-Operations-Generic-ROS-PDUs.asn \
			Remote-Operations-Useful-Definitions.asn 
	$(ASNC) $(ASNCFLAGS) $^

Remote-Operations-Information-Objects.erl:	\
			Remote-Operations-Information-Objects.asn \
			Remote-Operations-Generic-ROS-PDUs.asn \
			Remote-Operations-Useful-Definitions.asn
	$(ASNC) $(ASNCFLAGS) $^

.PHONY:	clean
clean:
	- rm -f *.asn1db 
	- rm -f TR.erl TCAPMessages.erl \
			DialoguePDUs.erl UnidialoguePDUs.erl \
			Remote-Operations-Useful-Definitions.erl \
			Remote-Operations-Information-Objects.erl \
			Remote-Operations-Generic-ROS-PDUs.erl \
			TC-Testing-User.erl TC-TMP.erl \
			TCAP-Examples.erl TCAP-Tools.erl \
			TC-Notation-Extensions.erl
	- rm -f Remote-Operations-Generic-ROS-PDUs.hrl \
			DialoguePDUs.hrl UnidialoguePDUs.hrl \
			TCAPMessages.hrl TR.hrl \
			TC-Testing-User.hrl TC-TMP.hrl \
			TCAP-Examples.hrl
	- rm -f $(EBIN)/TR.beam $(EBIN)/TCAPMessages.beam \
			$(EBIN)/DialoguePDUs.beam $(EBIN)/UnidialoguePDUs.beam \
			$(EBIN)/TC-Testing-User.beam \
			$(EBIN)/TCAP-Examples.beam $(EBIN)/TCAP-Tools.beam

