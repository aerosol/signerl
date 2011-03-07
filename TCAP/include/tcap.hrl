%%% $Id: tcap.hrl,v 1.1 2005/01/04 05:43:50 vances Exp $
%%%---------------------------------------------------------------------
%%% Copyright Motivity Telecom Inc. 2004
%%%
%%% All rights reserved. No part of this computer program(s) may be
%%% used, reproduced, stored in any retrieval system, or transmitted,
%%% in any form or by any means, electronic, mechanical, photocopying,
%%% recording, or otherwise without prior written permission of
%%% Motivity Telecom Inc.
%%%---------------------------------------------------------------------

%%%
%%%  ITU-T recommendation Q.771 Functional Decsription of 
%%%  Transaction Capabilities describes the primitives and
%%%  their parameters used in the TC-Service and TR-Service
%%%  interfaces.  Each primitive has a record defined here
%%%  containing it's parameters.  Modules using the these
%%%  services utilize these records to format messages:
%%%
%%%  {'TC', 'BEGIN', request, Begin} when is_record(Begin, 'TC-BEGIN')
%%%  {'TR', 'UNI', indication, Unidirectional} when is_record(Unidirectional, 'TR-UNI')
%%%

%%%
%%%  TC-User
%%%
%%% 	componentsPresent is a boolean

%% reference: Table 3/Q.771 - TC-UNI primitives
-record('TC-UNI', {qos, destAddress, appContextName, origAddress, dialogueID,
		userInfo, componentsPresent}).

%% reference: Table 4/Q.771 - TC-BEGIN primitives
-record('TC-BEGIN', {qos, destAddress, appContextName, origAddress, dialogueID,
		userInfo, componentsPresent}).

%% reference: Table 5/Q.771 - TC-CONTINUE primitives
-record('TC-CONTINUE', {qos, origAddress, appContextName, dialogueID,
		userInfo, componentsPresent}).

%% reference: Table 7/Q.771 - TC-END primitives
-record('TC-END', {qos, dialogueID, appContextName, componentsPresent,
		userInfo, termination}).

%% reference: Table 8/Q.771 - TC-U-ABORT primitives
-record('TC-U-ABORT', {qos, dialogueID, abortReason, appContextName, userInfo}).
%% abortReason is one of applicationContextNotSupported, dialogueRefused, userSpecific

%% reference: Table 9/Q.771 - TC-NOTICE primitives
-record('TC-NOTICE', {dialogueID, origAddress, destAddress, reportCause}).

%% reference: Table 10/Q.771 - Operation invocation primitives
-record('TC-INVOKE', {dialogueID, class, invokeID, linkedID, operation,
		parameters, lastComponent, timeout}).

%% reference: Table 11/Q.771 - Report of success primitives
-record('TC-RESULT-L', {dialogueID, invokeID, operation, parameters,
		lastComponent}).
-record('TC-RESULT-NL', {dialogueID, invokeID, operation, parameters,
		lastComponent}).

%% reference: Table 12/Q.771 - Report of failure primitives
-record('TC-U-ERROR', {dialogueID, invokeID, error, parameters,
		lastComponent}).

%% reference: Table 13/Q.771 - User rejection primitives
-record('TC-U-REJECT', {dialogueID, invokeID, problemCode,
		lastComponent}).

%% reference: Table 14/Q.771 - TC-CANCEL primitives
-record('TC-L-CANCEL', {dialogueID, invokeID}).
-record('TC-U-CANCEL', {dialogueID, invokeID}).

%% reference: Table 14bis/Q.771 TC-TIMER-RESET primitives
-record('TC-TIMER-RESET', {dialogueID, invokeID}).

%% reference: Table 15/Q.771 - Component sublayer rejection primitives
-record('TC-L-REJECT', {dialogueID, invokeID, problemCode, lastComponent}).
-record('TC-R-REJECT', {dialogueID, invokeID, problemCode, lastComponent}).

%% reference: Table 16/Q.771 - Primitive for Abort
-record('TC-P-ABORT', {qos, dialogueID, pAbort}).
%% pAbort is either a P-AbortCause or abnormalDialogue or noCommonDialoguePortion


%%%
%%%  TR-User
%%%
%%% userData is a 'TR-user-data' record
%%%
%%% Q.771 does not provide a distinction between the dialogue portion and 
%%% component portion within the user data parameter.  In 3.1.1 it says
%%%    "a dialogue portion is formatted and sent concatenated with the 
%%%     (sequence of) components(s)."
%%% This is probably due to dialogue handling being added after Q.771 was
%%% first written.  We will define a record for the user data.
-record('TR-user-data', {dialoguePortion, componentPortion}).

%% reference: Table 18/Q.771 - TR-UNI primitives
-record('TR-UNI', {qos, destAddress, origAddress, userData}).

%% reference: Table 19/Q.771 - Primitives for transaction begin
-record('TR-BEGIN', {qos, destAddress, origAddress, transactionID, userData}).

%% reference: Table 20/Q.771 - Transaction continuation primitives
-record('TR-CONTINUE', {qos, origAddress, transactionID, userData}).

%% reference: Table 22/Q.771 - TR-END primitives
-record('TR-END', {qos, transactionID, termination, userData}).

%% reference: Table 23/Q.771 - TR-U-ABORT primitives
-record('TR-U-ABORT', {qos, transactionID, userData}).

%% reference: Table 24/Q.771 - Transaction sublayer abort primitive
-record('TR-P-ABORT', {qos, transactionID, pAbort}).
%% pAbort is a P-AbortCause

%% reference: Table 25/Q.771 - TR-NOTICE primitive
-record('TR-NOTICE', {transactionID, origAddress, destAddress, reportCause}).

