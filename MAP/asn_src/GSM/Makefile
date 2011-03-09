
## I found it necessary to copy Remote-Operations-* here
## TCAPINCLUDES	= -I ../../../TCAP/itu/asn_src
TCAPINCLUDES	= 

ERLC			= erlc
ERLCFLAGS	= -b beam -W -v

ASNC			= $(ERLC) 
ASNCFLAGS	= -W -b ber_bin $(TCAPINCLUDES) +optimize +driver

%.beam:%.erl
	$(ERLC) $(ERLCFLAGS) $<

.SECONDARY:	%.erl
%.erl:%.asn
	$(ASNC) $(ASNCFLAGS) $<

default:	all

all:	MAP-SecureTransportOperations.beam \
			MAP-MobileServiceOperations.beam \
			MAP-LocationServiceOperations.beam \
			MAP-OperationAndMaintenanceOperations.beam \
			MAP-CallHandlingOperations.beam \
			MAP-SupplementaryServiceOperations.beam \
			MAP-ShortMessageServiceOperations.beam \
			MAP-Group-Call-Operations.beam \
			MAP-ApplicationContexts.beam \
			MAP-DialogueInformation.beam \
			MAP-Protocol.beam


MAP-CommonDataTypes.erl:	MAP-ExtensionDataTypes.erl \
			MAP-TS-Code.erl MAP-BS-Code.erl MAP-SS-Code.erl
MAP-ER-DataTypes.erl:	MAP-ExtensionDataTypes.erl \
			MAP-SS-DataTypes.erl MAP-CommonDataTypes.erl \
			MAP-ST-DataTypes.erl MAP-SS-Code.erl
MAP-ST-DataTypes.erl:	MAP-CommonDataTypes.erl
MAP-MS-DataTypes.erl:	MAP-ER-DataTypes.erl \
			MAP-SS-DataTypes.erl MAP-SS-Code.erl \
			MAP-BS-Code.erl MAP-TS-Code.erl \
			MAP-CommonDataTypes.erl MAP-ExtensionDataTypes.erl
MAP-LCS-DataTypes.erl:	MAP-SM-DataTypes.erl \
			MAP-CommonDataTypes.erl MAP-ExtensionDataTypes.erl \
			MAP-SS-DataTypes.erl MAP-MS-DataTypes.erl
MAP-CH-DataTypes.erl:	MAP-ExtensionDataTypes.erl \
			MAP-MS-DataTypes.erl MAP-SS-DataTypes.erl \
			MAP-CommonDataTypes.erl
MAP-OM-DataTypes.erl:	MAP-ExtensionDataTypes.erl \
			MAP-CommonDataTypes.erl
MAP-SS-DataTypes.erl:	MAP-SS-Code.erl \
			MAP-CommonDataTypes.erl MAP-ExtensionDataTypes.erl
MAP-SM-DataTypes.erl:	MAP-ExtensionDataTypes.erl \
			MAP-CommonDataTypes.erl MAP-ER-DataTypes.erl
MAP-GR-DataTypes.erl:	MAP-ExtensionDataTypes.erl \
			MAP-CommonDataTypes.erl MAP-TS-Code.erl \
			MAP-MS-DataTypes.erl

MAP-Errors.erl:	MAP-ER-DataTypes.erl \
			MAP-SS-DataTypes.erl

MAP-SecureTransportOperations.erl:	MAP-ST-DataTypes.erl \
			MAP-Errors.erl
MAP-MobileServiceOperations.erl:	MAP-MS-DataTypes.erl \
			MAP-Errors.erl
MAP-LocationServiceOperations.erl:	MAP-LCS-DataTypes.erl \
			MAP-Errors.erl
MAP-OperationAndMaintenanceOperations.erl:	MAP-CommonDataTypes.erl \
			MAP-Errors.erl MAP-OM-DataTypes.erl
MAP-CallHandlingOperations.erl:	MAP-CH-DataTypes.erl \
			MAP-Errors.erl
MAP-SupplementaryServiceOperations.erl:	MAP-SS-Code.erl \
			MAP-Errors.erl MAP-SS-DataTypes.erl
MAP-ShortMessageServiceOperations.erl:	MAP-SM-DataTypes.erl \
			MAP-Errors.erl
MAP-Group-Call-Operations.erl:	MAP-GR-DataTypes.erl \
			MAP-Errors.erl

MAP-ApplicationContexts.erl:	MobileDomainDefinitions.erl
MAP-DialogueInformation.erl:	MAP-ST-DataTypes.erl \
			MobileDomainDefinitions.erl MAP-CommonDataTypes.erl \
			MAP-ExtensionDataTypes.erl
MAP-Protocol.erl:	MAP-MobileServiceOperations.erl \
			MAP-OperationAndMaintenanceOperations.erl \
			MAP-CallHandlingOperations.erl \
			MAP-SupplementaryServiceOperations.erl \
			MAP-ShortMessageServiceOperations.erl \
			MAP-Group-Call-Operations.erl \
			MAP-LocationServiceOperations.erl \
			MAP-SecureTransportOperations.erl

.PHONY:	clean
clean:
	rm -rf *.asn1db 
	rm -rf MobileDomainDefinitions.erl \
			MAP-ApplicationContexts.erl MAP-BS-Code.erl \
			MAP-CH-DataTypes.erl MAP-CallHandlingOperations.erl \
			MAP-CommonDataTypes.erl MAP-DialogueInformation.erl \
			MAP-ER-DataTypes.erl MAP-Errors.erl MAP-ExtensionDataTypes.erl \
			MAP-GR-DataTypes.erl MAP-Group-Call-Operations.erl \
			MAP-LCS-DataTypes.erl MAP-LocationServiceOperations.erl \
			MAP-MS-DataTypes.erl MAP-MobileServiceOperations.erl \
			MAP-OM-DataTypes.erl MAP-OperationAndMaintenanceOperations.erl \
			MAP-Protocol.erl MAP-SM-DataTypes.erl \
			MAP-SS-Code.erl MAP-SS-DataTypes.erl MAP-ST-DataTypes.erl \
			MAP-SecureTransportOperations.erl \
			MAP-ShortMessageServiceOperations.erl \
			MAP-SupplementaryServiceOperations.erl MAP-TS-Code.erl
	rm -rf MAP-ApplicationContexts.hrl MAP-MS-DataTypes.hrl \
			MAP-CH-DataTypes.hrl MAP-OM-DataTypes.hrl \
			MAP-DialogueInformation.hrl MAP-SM-DataTypes.hrl \
			MAP-GR-DataTypes.hrl MobileDomainDefinitions.hrl \
			MAP-LCS-DataTypes.hrl MAP-BS-Code.hrl \
			MAP-CommonDataTypes.hrl MAP-ER-DataTypes.hrl \
			MAP-ExtensionDataTypes.hrl MAP-SS-Code.hrl \
			MAP-SS-DataTypes.hrl MAP-ST-DataTypes.hrl \
			MAP-TS-Code.hrl
	rm -rf *.beam
