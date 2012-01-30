%%% $Id: tcap_dha_fsm.erl,v 1.3 2005/08/04 09:33:17 vances Exp $
%%%---------------------------------------------------------------------
%%% @copyright 2004-2005 Motivity Telecom, 2010-2012 Harald Welte
%%% @author Vance Shipley <vances@motivity.ca>, Harald Welte <laforge@gnumonks.org>
%%% @end
%%%
%%% Copyright (c) 2004-2005, Motivity Telecom
%%% Copyright (c) 2010-2012, Harald Welte <laforge@gnumonks.org>
%%% 
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%% 
%%%    - Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    - Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in
%%%      the documentation and/or other materials provided with the 
%%%      distribution.
%%%    - Neither the name of Motivity Telecom nor the names of its
%%%      contributors may be used to endorse or promote products derived
%%%      from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%
%%%---------------------------------------------------------------------
%%%
%%% @doc Dialogue Handler (DHA) functional block within the component
%%% 		sub-layer of ITU TCAP.
%%%
%%% @reference ITU-T Q.774 (06/97) Annex A Transaction capabilities SDLs
%%%
%%% @private
%%%

-module(tcap_dha_fsm).
-copyright('Copyright (c) 2004-2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision: 1.3 $').

-behaviour(gen_fsm).

%% call backs needed for gen_fsm behaviour
-export([init/1, handle_info/3, handle_event/3, handle_sync_event/4,
		terminate/3, code_change/4]).

%% transaction_fsm state callbacks 
-export([idle/2, wait_for_uni_components/2, wait_for_begin_components/2,
		initiation_received/2, wait_cont_components_ir/2,
		wait_cont_components_active/2, wait_for_end_components/2,
		initiation_sent/2, active/2]).

-export([get_cco_pid/1]).

%% record definitions for TR-User primitives
-include("tcap.hrl").
%% record definitions for N-User primitives
-include("sccp.hrl").
%% record definitions for TCAP messages
%-include("TCAPMessages.hrl").
-include("UnidialoguePDUs.hrl").
-include("DialoguePDUs.hrl").

%% the dialogue_fsm state data
-record(state, {usap, tco, supid, cco, otid, did, parms, appContextMode}).

%%----------------------------------------------------------------------
%%  The gen_fsm call backs
%%----------------------------------------------------------------------

%% Start the Dialogue Handler (DHA) process
%% reference: Figure A.5/Q.774 (sheet 1 of 11)
init({USAP, DialogueID, TCO}) ->
	init({USAP, DialogueID, TCO, undefined});
init({USAP, DialogueID, TCO, SupId}) ->
	ets:insert(tcap_dha, {DialogueID, self()}),
	CCO = list_to_atom("tcap_cco_" ++ integer_to_list(DialogueID)),
	process_flag(trap_exit, true),
	{ok, idle, #state{usap = USAP, did = DialogueID,
			tco = TCO, supid = SupId, cco = CCO}}.

%% reference: Figure A.5/Q.774 (sheet 1 of 11)
%%% TC-UNI request from TCU
idle({'TC', 'UNI', request, UniParms}, State) 
		when is_record(UniParms, 'TC-UNI') ->
	%% Dialogue info included?
	case UniParms#'TC-UNI'.userInfo of
		undefined ->
			DialoguePortion = undefined;
		UserInfo when is_binary(UserInfo) ->
			%% Build AUDT apdu
			DialoguePortion = 'UnidialoguePDUs':encode('AUDT-apdu',
					#'AUDT-apdu'{'application-context-name' = UniParms#'TC-UNI'.appContextName,
					'user-information' = UserInfo})
	end,
	TrParms = #'TR-UNI'{qos = UniParms#'TC-UNI'.qos,
			destAddress = UniParms#'TC-UNI'.destAddress,
			origAddress = UniParms#'TC-UNI'.origAddress,
			userData = #'TR-user-data'{dialoguePortion = dialogue_ext(DialoguePortion)}},
	NewState = State#state{parms = TrParms},
	%% Request components to CHA
	gen_server:cast(NewState#state.cco, 'request-components'),
	%% Process components
	{next_state, wait_for_uni_components, NewState};

%% reference: Figure A.5/Q.774 (sheet 1 of 11)
%%% TC-BEGIN request from TCU
idle({'TC', 'BEGIN', request, BeginParms}, State) 
		when is_record(BeginParms, 'TC-BEGIN') ->
	%% Dialogue info included?
	case BeginParms#'TC-BEGIN'.appContextName of
		undefined ->
			DialoguePortion = undefined;
		ACtx ->
			UserInfo = osmo_util:asn_val(BeginParms#'TC-BEGIN'.userInfo),
			%% Set protocol version = 1
			%% Build AARQ apdu
			{ok, DialoguePortion} = 'DialoguePDUs':encode('AARQ-apdu',
					#'AARQ-apdu'{'protocol-version' = [version1],
					'application-context-name' = ACtx,
					'user-information' = UserInfo})
	end,
	TrParms = #'TR-BEGIN'{qos = BeginParms#'TC-BEGIN'.qos,
			destAddress = BeginParms#'TC-BEGIN'.destAddress,
			origAddress = BeginParms#'TC-BEGIN'.origAddress,
			userData = #'TR-user-data'{dialoguePortion = dialogue_ext(DialoguePortion)}},
	NewState = State#state{parms = TrParms,
			%% Set application context mode
			appContextMode = BeginParms#'TC-BEGIN'.appContextName},
	%% Request components to CHA
	gen_server:cast(NewState#state.cco, 'request-components'),
	%% Process components
	{next_state, wait_for_begin_components, NewState};

%% reference: Figure A.5/Q.774 (sheet 2 of 11)
%%% TR-UNI indication from TSL
idle({'TR', 'UNI', indication, UniParms}, State) when is_record(UniParms, 'TR-UNI') ->
	%% Extract dialogue portion
	case extract_uni_dialogue_portion(UniParms#'TR-UNI'.userData) of
		incorrect_dialogue_portion ->       %% Dialogue portion correct? (no)
			%% Discard components
			{stop, normal, State};
		no_version1 ->                     %% Is version 1 supported? (no)
			%% Discard components
			{stop, normal, State};
		TcParms when is_record(TcParms, 'TC-UNI') ->
			if
				is_record(UniParms#'TR-UNI'.userData, 'TR-user-data'),
						(UniParms#'TR-UNI'.userData)#'TR-user-data'.componentPortion /= asn1_NOVALUE ->
					case 'TC':decode('Components', (UniParms#'TR-UNI'.userData)#'TR-user-data'.componentPortion) of
						[] = Components -> ComponentsPresent = false;
						Components -> ComponentsPresent = true
					end;
				true ->
					Components = undefined,
					ComponentsPresent = false
			end,
			%% Assign dialogue ID
			DialogueID = tcap_tco_server:new_tid(),
			NewTcParms = TcParms#'TC-UNI'{qos = UniParms#'TR-UNI'.qos,
					destAddress = UniParms#'TR-UNI'.destAddress,
					origAddress = UniParms#'TR-UNI'.origAddress,
					dialogueID = DialogueID,
					componentsPresent = ComponentsPresent},
			NewState = State#state{did = DialogueID, parms = NewTcParms},
			%% Components to CHA
			case ComponentsPresent of
				true ->
					gen_server:cast(NewState#state.cco, {components, Components});
				false ->
					ok       % should never happen
			end,
			%% TC-UNI indication to TCU
			gen_fsm:send_event(NewState#state.usap, {'TC', 'UNI', indication, NewTcParms}),
			%% Dialogue terminated to CHA
			gen_server:cast(NewState#state.cco, 'dialogue-terminated'),
			%% Free dialogue ID
			{stop, normal, NewState}
	end;

%% reference: Figure A.5/Q.774 (sheet 3 of 11)
%%% TR-BEGIN indication from TSL
idle({'TR', 'BEGIN', indication, BeginParms}, State) when is_record(BeginParms, 'TR-BEGIN') ->
	%% Extract dialogue portion
	case extract_begin_dialogue_portion(BeginParms#'TR-BEGIN'.userData) of
		incorrect_dialogue_portion ->       %% Dialogue portion correct? (no)
			%% Build ABORT apdu
			ABRT = 'DialoguePDUs':encode('ABRT-apdu', #'ABRT-apdu'{'abort-source' = 'dialogue-service-provider'}),
			%% Discard components
			%% TR-U-ABORT request to TSL
			TrParms = {transactionID = BeginParms#'TR-BEGIN'.transactionID,
					userData = #'TR-user-data'{dialoguePortion = dialogue_ext(ABRT)}},
			NewState = State#state{otid = BeginParms#'TR-BEGIN'.transactionID, parms = TrParms},
			gen_server:cast(NewState#state.tco, {'TR', 'U-ABORT', request, TrParms}),
			%% Dialogue terminated to CHA
			gen_server:cast(NewState#state.cco, 'dialogue-terminated'),
			{stop, normal, NewState};
		no_version1 ->                     %% Is version 1 supported? (no)
			DialoguePortion = (BeginParms#'TR-BEGIN'.userData)#'TR-user-data'.dialoguePortion,
			%% Build AARE apdu
			AARE = 'DialoguePDUs':encode('AARE-apdu', #'AARE-apdu'{
					'protocol-version' = version1,
					'application-context-name' = DialoguePortion#'AARQ-apdu'.'application-context-name',
					result = reject-permanent,
					'result-source-diagnostic' = {'dialogue-service-provider', 'no-common-dialogue-portion'}}),
			%% Discard components
			%% TR-P-ABORT request to TSL
			TrParms = {transactionID = BeginParms#'TR-P-ABORT'.transactionID, pAbort = AARE},
			NewState = State#state{otid = BeginParms#'TR-BEGIN'.transactionID,
					appContextMode = DialoguePortion#'AARQ-apdu'.'application-context-name',
					parms = TrParms},
			gen_server:cast(NewState#state.tco, {'TR', 'P-ABORT', request, TrParms}),
			%% Dialogue terminated to CHA
			gen_server:cast(NewState#state.cco, 'dialogue-terminated'),
			{stop, normal, NewState};
		TcParms when is_record(TcParms, 'TC-BEGIN') ->
			if
				is_record(BeginParms#'TR-BEGIN'.userData, 'TR-user-data'),
						(BeginParms#'TR-BEGIN'.userData)#'TR-user-data'.componentPortion /= asn1_NOVALUE ->
					case 'TC':decode('Components', (BeginParms#'TR-BEGIN'.userData)#'TR-user-data'.componentPortion) of
						[] = Components -> ComponentsPresent = false;
						Components -> ComponentsPresent = true
					end;
				true ->
					Components = undefined,
					ComponentsPresent = false
			end,
			%% Assign dialogue ID
			DialogueID = tcap_tco_server:new_tid(),
			NewTcParms = TcParms#'TC-BEGIN'{qos = BeginParms#'TR-BEGIN'.qos,
					destAddress = BeginParms#'TR-BEGIN'.destAddress,
					origAddress = BeginParms#'TR-BEGIN'.origAddress,
					dialogueID = DialogueID,
					componentsPresent = ComponentsPresent},
			NewState = State#state{otid = BeginParms#'TR-BEGIN'.transactionID, did = DialogueID,
					parms = NewTcParms, appContextMode = TcParms#'TC-BEGIN'.appContextName},
			%% TC-BEGIN indication to TCU
			gen_fsm:send_event(NewState#state.usap, {'TC', 'BEGIN', indication, NewTcParms}),
			%% Any components?
			case ComponentsPresent of
				true ->
					%% Components to CHA
					gen_server:cast(NewState#state.cco, {components, Components});
				false ->
					ok
			end,
			{next_state, initiation_received, NewState}
	end.


%% reference: Figure A.5/Q.774 (sheet 5 of 11)
%%% TC-CONTINUE request from TCU
initiation_received({'TC', 'CONTINUE', request, ContParms}, State) when is_record(ContParms, 'TC-CONTINUE') ->
	%% Dialogue info included?
	case ContParms#'TC-CONTINUE'.userInfo of
		UserInfo when is_binary(UserInfo) ->
			AARE = #'AARE-apdu'{'protocol-version' = version1,
					'application-context-name' = ContParms#'TC-CONTINUE'.appContextName,
					result = accepted,
					'result-source-diagnostic' = {'dialogue-service-user', null},
					'user-information' = UserInfo},
			DialoguePortion = 'DialoguePDUs':encode('AARE-apdu', AARE),
			TrParms = #'TR-CONTINUE'{qos = ContParms#'TC-CONTINUE'.qos,
					origAddress = ContParms#'TR-CONTINUE'.origAddress,
					transactionID = State#state.otid,
					userData = #'TR-user-data'{dialoguePortion = dialogue_ext(DialoguePortion)}},
			NewState = State#state{parms = TrParms};
		undefined ->
			NewState = State
	end,
	{next_state, wait_cont_components_ir, NewState};

%% reference: Figure A.5/Q.774 (sheet 5 of 11)
%%% TC-END request from TCU
initiation_received({'TC', 'END', request, EndParms}, State) when is_record(EndParms, 'TC-END') ->
	%% Prearranged end?
	case EndParms#'TC-END'.termination of
		prearranged ->
			%% TR-END request to TSL
			TrParms = #'TR-END'{qos = EndParms#'TC-END'.qos,
					transactionID = State#state.otid,
					termination = EndParms#'TC-END'.termination},
			NewState = State#state{parms = TrParms},
			gen_server:cast(NewState#state.tco, {'TR', 'END', request, TrParms}),
			%% Dialogue terminated to CHA
			gen_server:cast(NewState#state.cco, 'dialogue-terminated'),
			%% Free dialogue ID
			{stop, normal, NewState};
		basic ->
			%% Dialogue info included?
			case EndParms#'TC-END'.userInfo of
				UserInfo when is_binary(UserInfo) ->
					AARE = #'AARE-apdu'{'protocol-version' = version1,
							'application-context-name' = EndParms#'TC-END'.appContextName,
							result = accepted,
							'result-source-diagnostic' = {'dialogue-service-user', null},
							'user-information' = UserInfo},
					DialoguePortion = 'DialoguePDUs':encode('AARE-apdu', AARE),
					TrParms = #'TR-END'{qos = EndParms#'TC-END'.qos,
							transactionID = State#state.otid,
							termination = EndParms#'TC-END'.termination,
							userData = #'TR-user-data'{dialoguePortion =
											dialogue_ext(DialoguePortion)}},
					NewState = State#state{parms = TrParms};
				undefined ->
					NewState = State
			end,
			%% Request components to CHA
			gen_server:cast(NewState#state.cco, 'request-components'),
			%% Process components
			{next_state, wait_for_end_components, NewState}
	end;

%% reference: Figure A.5/Q.774 (sheet 6 of 11)
%%% TC-U-ABORT request from TCU
initiation_received({'TC', 'U-ABORT', request, AbortParms}, State) when is_record(AbortParms, 'TC-U-ABORT'),
		(AbortParms#'TC-U-ABORT'.abortReason == applicationContextNotSupported)
		or (AbortParms#'TC-U-ABORT'.abortReason == dialogueRefused)
		or (AbortParms#'TC-U-ABORT'.abortReason == userSpecified) ->
	case State#state.appContextMode of
		%% Is application context mode set? (no)
		undefined ->
			UserData = #'TR-user-data'{};
		%% Abort reason present and = AC-name not supported OR dialogue refused?
		_AppContextName when AbortParms#'TC-U-ABORT'.abortReason == applicationContextNotSupported ->
			%% Set protocol version = 1
			%% Build AARE-pdu (rejected)
			AARE = 'DialoguePDUs':encode('AARE-apdu',
					#'AARE-apdu'{'protocol-version' = version1,
					'application-context-name' = AbortParms#'TC-U-ABORT'.appContextName,
					result = 'reject-permanent',
					'result-source-diagnostic' = {'dialogue-service-user', 'application-context-name-not-supported'}}),
			UserData = #'TR-user-data'{dialoguePortion = dialogue_ext(AARE)};
		_AppContextName when AbortParms#'TC-U-ABORT'.abortReason == dialogueRefused ->
			%% Set protocol version = 1
			%% Build AARE-pdu (rejected)
			AARE = 'DialoguePDUs':encode('AARE-apdu',
					#'AARE-apdu'{'protocol-version' = version1,
					'application-context-name' = AbortParms#'TC-U-ABORT'.appContextName,
					result = 'reject-permanent',
					'result-source-diagnostic' = {'dialogue-service-user', null}}),
			UserData = #'TR-user-data'{dialoguePortion = dialogue_ext(AARE)};
		_AppContextName when AbortParms#'TC-U-ABORT'.abortReason == userSpecified ->
			%% Build ABRT-apdu (abort source = dialogue-service-user)
			ABRT = 'DialoguePDUs':encode('ABRT-apdu',
					#'ABRT-apdu'{'abort-source' = 'dialogue-service-user',
					'user-information' = AbortParms#'TC-U-ABORT'.userInfo}),
			UserData = #'TR-user-data'{dialoguePortion = dialogue_ext(ABRT)}
	end,
	%% TR-U-ABORT request to TSL
	TrParms = #'TR-U-ABORT'{qos = AbortParms#'TC-U-ABORT'.qos,
			transactionID = State#state.otid,
			userData = UserData},
	NewState = State#state{parms = TrParms},
	gen_server:cast(NewState#state.tco, {'TR', 'U-ABORT', request, TrParms}),
	%% Dialogue terminated to CHA
	gen_server:cast(NewState#state.cco, 'dialogue-terminated'),
	%% Free dialogue ID
	{stop, normal, NewState}.

%% reference: Figure A.5/Q.774 (sheet 7 of 11)
%%% TC-END request from TCU
initiation_sent({'TC', 'END', request, EndParms}, State) when is_record(EndParms, 'TC-END'),
		EndParms#'TC-END'.termination == prearranged ->   % termination must be prearranged
	%% TR-END request to TSL
	TrParms = #'TR-END'{qos = EndParms#'TC-END'.qos,
			transactionID = State#state.otid,
			termination = EndParms#'TC-END'.termination},
	NewState = State#state{parms = TrParms},
	gen_server:cast(NewState#state.tco, {'TR', 'END', request, TrParms}),
	%% Dialogue terminated to CHA
	gen_server:cast(NewState#state.cco, 'dialogue-terminated'),
	{stop, normal, NewState};

%% reference: Figure A.5/Q.774 (sheet 7 of 11)
%%% TC-U-ABORT request from TCU (local action)
initiation_sent({'TC', 'U-ABORT', request, AbortParms}, State) when is_record(AbortParms, 'TC-U-ABORT') ->
	%% TR-U-ABORT request to TSL
	TrParms = #'TR-U-ABORT'{qos = AbortParms#'TC-U-ABORT'.qos, transactionID = State#state.otid},
	NewState = State#state{parms = TrParms},
	gen_server:cast(NewState#state.tco, {'TR', 'U-ABORT', request, TrParms}),
	%% Dialogue terminated to CHA
	gen_server:cast(NewState#state.cco, 'dialogue-terminated'),
	{stop, normal, NewState};

%% reference: Figure A.5/Q.774 (sheet 7 of 11)
%%% TR-END indication from TSL
initiation_sent({'TR', 'END', indication, EndParms}, State) when is_record(EndParms, 'TR-END') ->
	if
		is_record(EndParms#'TR-END'.userData, 'TR-user-data'),
				(EndParms#'TR-END'.userData)#'TR-user-data'.componentPortion /= asn1_NOVALUE ->
			case 'TC':decode('Components', (EndParms#'TR-END'.userData)#'TR-user-data'.componentPortion) of
				[] = Components -> ComponentsPresent = false;
				Components -> ComponentsPresent = true
			end;
		true ->
			Components = undefined,
			ComponentsPresent = false
	end,
	%% Dialogue portion included?
	%% AC Mode set?
	%% Extract dialogue portion
	%% Dialogue portion correct?
	case extract_dialogue_portion(EndParms#'TR-END'.userData, State#state.appContextMode) of
		abort ->
			%% Discard components
			%% TC-P-ABORT indication to TCU
			TcParms = #'TC-P-ABORT'{qos = EndParms#'TR-END'.qos,
					dialogueID = State#state.did,
					pAbort = abnormalDialogue},
			NewState = State#state{parms = TcParms},
			gen_fsm:send_event(NewState#state.usap, {'TC', 'P-ABORT', indication, TcParms}),
			%% Dialogue terminated to CHA
			gen_server:cast(NewState#state.cco, 'dialogue-terminated'),
			%% Free dialogue ID
			{stop, normal, NewState};
		AARE ->
			%% TC-END indication to TCU
			TcParms = #'TC-END'{qos = EndParms#'TR-END'.qos,
					dialogueID = State#state.did,
					appContextName = State#state.appContextMode,
					componentsPresent = ComponentsPresent,
					userInfo = AARE,
					termination = EndParms#'TR-END'.termination},
			NewState = State#state{parms = TcParms},
			gen_fsm:send_event(NewState#state.usap, {'TC', 'END', indication, TcParms}),
			%% Any components?
			case ComponentsPresent of
				true ->
					%% Components to CHA
					gen_server:cast(NewState#state.cco, {components, Components});
				false ->
					ok
			end,
			%% Dialogue terminated to CHA
			gen_server:cast(NewState#state.cco, 'dialogue-terminated'),
			%% Free dialogue ID
			{stop, normal, NewState}
	end;

%% reference: Figure A.5/Q.774 (sheet 7 of 11)
%% NOTE:  currently the TCO short circuits this function and sends directly to TCU
initiation_sent({'TR', 'NOTICE', indication, NoticeParms}, State) when is_record(NoticeParms, 'TR-NOTICE') ->
	%% TC-NOTICE indication to TCU
	TcParms = #'TC-NOTICE'{dialogueID = State#state.did,
			origAddress = NoticeParms#'TR-NOTICE'.origAddress,
			destAddress = NoticeParms#'TR-NOTICE'.destAddress,
			reportCause = NoticeParms#'TR-NOTICE'.reportCause},
	NewState = State#state{parms = TcParms},
	gen_fsm:send_event(NewState#state.usap, {'TC', 'NOTICE', indication, TcParms}),
	{next_state, initiation_sent, NewState};

%% reference: Figure A.5/Q.774 (sheet 8 of 11)
%% TR-CONTINUE indication from TSL
initiation_sent({'TR', 'CONTINUE', indication, ContParms}, State) when is_record(ContParms, 'TR-CONTINUE') ->
	if
		is_record(ContParms#'TR-CONTINUE'.userData, 'TR-user-data'),
				(ContParms#'TR-CONTINUE'.userData)#'TR-user-data'.componentPortion /= asn1_NOVALUE ->
			case 'TC':decode('Components', (ContParms#'TR-CONTINUE'.userData)#'TR-user-data'.componentPortion) of
				{ok, [] = Components} -> ComponentsPresent = false;
				{ok, Components} -> ComponentsPresent = true
			end;
		true ->
			Components = undefined,
			ComponentsPresent = false
	end,
	%% Dialogue portion included?
	%% AC Mode set?
	%% Extract dialogue portion
	%% Dialogue portion correct?
	io:format("Components: ~p\n", [Components]),
	io:format("Dialogue: ~p\n", [(ContParms#'TR-CONTINUE'.userData)#'TR-user-data'.dialoguePortion]),
	case extract_dialogue_portion(ContParms#'TR-CONTINUE'.userData, State#state.appContextMode) of
		abort ->
			%% Discard components
			%% TC-P-ABORT indication to TCU
			TcParms = #'TC-P-ABORT'{qos = ContParms#'TR-CONTINUE'.qos,
					dialogueID = State#state.did,
					pAbort = abnormalDialogue},
			NewState = State#state{parms = TcParms},
			gen_fsm:send_event(NewState#state.usap, {'TC', 'P-ABORT', indication, TcParms}),
			%% Build ABRT apdu
			ABRT = 'DialoguePDUs':encode('ABRT-apdu',
					#'ABRT-apdu'{'abort-source' = 'dialogue-service-provider'}),
			UserData = #'TR-user-data'{dialoguePortion = dialogue_ext(ABRT)},
			%% TR-U-ABORT request to TSL
			TrParms = #'TR-U-ABORT'{qos = ContParms#'TC-U-ABORT'.qos,
					transactionID = NewState#state.otid, userData = UserData},
			LastState = State#state{parms = TrParms},
			gen_server:cast(LastState#state.tco, {'TR', 'U-ABORT', request, TrParms}),
			%% Dialogue terminated to CHA
			gen_server:cast(LastState#state.cco, 'dialogue-terminated'),
			%% Free dialogue ID
			{stop, normal, LastState};
		AARE ->
			%% TC-CONTINUE indication to TCU
			TcParms = #'TC-CONTINUE'{qos = ContParms#'TR-CONTINUE'.qos,
					origAddress = ContParms#'TR-CONTINUE'.origAddress,
					appContextName = State#state.appContextMode,
					dialogueID = State#state.did,
      			userInfo = AARE,
					componentsPresent = ComponentsPresent},
			NewState = State#state{parms = TcParms},
			gen_fsm:send_event(NewState#state.usap, {'TC', 'CONTINUE', indication, TcParms}),
			%% Any components?
			case ComponentsPresent of
				true ->
					%% Components to CHA
					gen_server:cast(NewState#state.cco, {components, Components});
				false ->
					ok
			end,
			{next_state, active, NewState}
	end;

%% reference: Figure A.5/Q.774 (sheet 8 of 11)
%% TR-U-ABORT indication from TSL
initiation_sent({'TR', 'U-ABORT', indication, AbortParms}, State) when is_record(AbortParms, 'TR-U-ABORT') ->
	case catch begin
		if
			%% Is AC mode set? (no) Is Dialogue portion present? (no)
			State#state.appContextMode == undefined and (not is_record(AbortParms#'TR-U-ABORT'.userData, 'TR-user-data')
					or (AbortParms#'TR-U-ABORT'.userData)#'TR-user-data'.dialoguePortion == undefined) ->
				throw(#'TC-U-ABORT'{qos = AbortParms#'TR-U-ABORT'.qos, dialogueID = State#state.did});
			%% Is AC mode set? (no) Is Dialogue portion present? (yes)
			State#state.appContextMode == undefined, is_record(AbortParms#'TR-U-ABORT'.userData, 'TR-user-data'),
					AbortParms#'TR-U-ABORT'.userData /= undefined ->
				throw(#'TC-P-ABORT'{qos = AbortParms#'TR-U-ABORT'.qos, 
						dialogueID = State#state.did, pAbort = abnormalDialogue});
			%% Is User Data included in primitive? (no)
			not is_record(AbortParms#'TR-U-ABORT'.userData, 'TR-user-data');
					(AbortParms#'TR-U-ABORT'.userData)#'TR-user-data'.dialoguePortion == undefined ->
				throw(#'TC-P-ABORT'{qos = AbortParms#'TR-U-ABORT'.qos, 
						dialogueID = State#state.did, pAbort = abnormalDialogue});
			true -> ok
		end,
		%% Is PDU type = ABRT or AARE (rejected)?
		case 'DialoguePDUs':decode('DialoguePDU', (AbortParms#'TR-U-ABORT'.userData)#'TR-user-data'.dialoguePortion) of
			%% Is abstract syntax = dialogue-PDU AS? (no)
      	{dialoguePDU, APDU}  when is_record(APDU, 'AARE-apdu'),
					APDU#'AARE-apdu'.'application-context-name' /= State#state.appContextMode ->
				#'TC-P-ABORT'{qos = AbortParms#'TR-U-ABORT'.qos,
						dialogueID = State#state.did,
						pAbort = abnormalDialogue};
			%% Is Abort source = user? (yes)
      	{dialoguePDU, APDU}  when is_record(APDU, 'ABRT-apdu'),
					element(1, APDU#'ABRT-apdu'.'abort-source') == 'dialogue-service-user' ->
				#'TC-U-ABORT'{qos = AbortParms#'TR-U-ABORT'.qos,
						dialogueID = State#state.did,
						abortReason = userSpecific,
						userInfo = APDU#'ABRT-apdu'.'user-information'};
			%% Is Associate source = user? (yes)
      	{dialoguePDU, APDU}  when is_record(APDU, 'AARE-apdu'), APDU#'AARE-apdu'.'result-source-diagnostic'
					== {'dialogue-service-user', 'application-context-name-not-supported'},
					APDU#'AARE-apdu'.result == 'reject-permanent' ->
				#'TC-U-ABORT'{qos = AbortParms#'TR-U-ABORT'.qos,
						dialogueID = State#state.did,
						abortReason = applicationContextNotSupported,
						appContextName = APDU#'AARE-apdu'.'application-context-name',
						userInfo = APDU#'AARE-apdu'.'user-information'};
      	{dialoguePDU, APDU}  when is_record(APDU, 'AARE-apdu'), 
					element(1, APDU#'AARE-apdu'.'result-source-diagnostic') == 'dialogue-service-user',
					APDU#'AARE-apdu'.result == 'reject-permanent' ->
				#'TC-U-ABORT'{qos = AbortParms#'TR-U-ABORT'.qos,
						dialogueID = State#state.did,
						abortReason = dialogueRefused,
						appContextName = APDU#'AARE-apdu'.'application-context-name',
						userInfo = APDU#'AARE-apdu'.'user-information'};
			%% Is AARE (no common dialogue portion)?
      	{dialoguePDU, APDU}  when is_record(APDU, 'AARE-apdu'),
					APDU#'AARE-apdu'.'result-source-diagnostic' == {'dialogue-service-provider', 'no-common-dialogue-portion'} ->
				#'TC-P-ABORT'{qos = AbortParms#'TR-U-ABORT'.qos,
						dialogueID = State#state.did,
						pAbort = noCommonDialoguePortion};
			_ ->
				#'TC-P-ABORT'{qos = AbortParms#'TR-U-ABORT'.qos,
						dialogueID = State#state.did,
						pAbort = abnormalDialogue}
		end
	end of
		TcParms when is_record(TcParms, 'TC-U-ABORT') ->
			NewState = State#state{parms = TcParms},
			gen_fsm:send_event(NewState#state.usap, {'TC', 'U-ABORT', indication, TcParms});
		TcParms when is_record(TcParms, 'TC-P-ABORT') ->
			NewState = State#state{parms = TcParms},
			gen_fsm:send_event(NewState#state.usap, {'TC', 'P-ABORT', indication, TcParms})
	end,
	%% Dialogue terminated to CHA
	gen_server:cast(NewState#state.cco, 'dialogue-terminated'),
	%% Free dialogue ID
	{stop, normal, NewState};

%% reference: Figure A.5/Q.774 (sheet 8 of 11)
%% TR-P-ABORT indication from TSL
initiation_sent({'TR', 'P-ABORT', indication, AbortParms}, State) when is_record(AbortParms, 'TR-P-ABORT') ->
	TcParms = #'TC-P-ABORT'{qos = AbortParms#'TR-P-ABORT'.qos,
			dialogueID = State#state.did,
			pAbort = AbortParms#'TR-P-ABORT'.pAbort},
	NewState = State#state{parms = TcParms},
	%% TC-P-ABORT indication to TCU
	gen_fsm:send_event(NewState#state.usap, {'TC', 'P-ABORT', indication, TcParms}),
	%% Dialogue terminated to CHA
	gen_server:cast(NewState#state.cco, 'dialogue-terminated'),
	%% Free dialogue ID
	{stop, normal, NewState}.


%% reference: Figure A.5/Q.774 (sheet 9 of 11)
%% TC-CONTINUE request from TCU
active({'TC', 'CONTINUE', request, ContParms}, State) when is_record(ContParms, 'TC-CONTINUE') ->
	NewState = State#state{parms = ContParms},
	%% Request component to CHA
	gen_server:cast(NewState#state.cco, 'request-components'),
	%% Process components
	{next_state, wait_cont_components_active, NewState};

%% reference: Figure A.5/Q.774 (sheet 9 of 11)
%% TC-END request from TCU
active({'TC', 'END', request, EndParms}, State) when is_record(EndParms, 'TC-END') ->
	%% Prearranged end?
	case EndParms#'TC-END'.termination of
		prearranged ->
			%% TR-END request to TSL
			TrParms = #'TR-END'{qos = EndParms#'TC-END'.qos,
					transactionID = State#state.otid,
					termination = EndParms#'TC-END'.termination},
			NewState = State#state{parms = TrParms},
			gen_server:cast(NewState#state.tco, {'TR', 'END', request, TrParms}),
			%% Dialogue terminated to CHA
			gen_server:cast(NewState#state.cco, 'dialogue-terminated'),
			%% Free dialogue ID
			{stop, normal, NewState};
		basic ->
			%% Request component to CHA
			gen_server:cast(State#state.cco, 'request-components'),
			%% Process components
			{next_state, wait_for_end_components, State}
	end;

%% reference: Figuer A.5/Q774 (sheet 10 of 11)
%% TR-END indication from TSL
active({'TR', 'END', indication, EndParms}, State) when is_record(EndParms, 'TR-END') ->
	UserData = EndParms#'TR-END'.userData,
	if
		UserData#'TR-user-data'.dialoguePortion /= asn1_NOVALUE ->
			% discard components
			% TC-P-ABORT.ind to TCU
			ok;
		true ->
			ComponentPortion = UserData#'TR-user-data'.componentPortion,
			if
				ComponentPortion /= asn1_NOVALUE ->
					case 'TC':decode('Components', ComponentPortion) of
						[] = Components -> ComponentsPresent = false;
						Components -> ComponentsPresent = true
					end;
				true ->
					Components = undefined,
					ComponentsPresent = false
			end,
			%% TC-END indication to TCU
			TcParms = #'TC-END'{qos = EndParms#'TR-END'.qos,
					dialogueID = State#state.did,
					appContextName = State#state.appContextMode,
					componentsPresent = ComponentsPresent,
					termination = EndParms#'TR-END'.termination},
			NewState = State#state{parms = TcParms},

			%% Components To CHA
			gen_fsm:send_event(NewState#state.usap, {'TC', 'END', indication, TcParms}),
			case ComponentsPresent of
				true ->
					gen_server:cast(State#state.cco, {components, Components});
				_ ->
					ok
			end
	end,
	%% Dialogue terminated to CHA
	gen_server:cast(State#state.cco, 'dialogue-terminated'),
	%% Free dialogue ID
	{stop, normal, State}.



%% reference: Figure A.5 bis/Q.774
%% reference: Figure A.5/Q.774 (sheet 2 of 11)
wait_for_uni_components('no-component', State) ->
	wait_for_uni_components1(State);
wait_for_uni_components({'requested-components', Components}, State) ->
	%% Assemble component portion
	{ok, ComponentPortion} = 'TC':encode('Components', Components),
	%% Assemble TSL user data
	UserData = (State#state.parms)#'TR-UNI'.userData,
	NewUserData = UserData#'TR-user-data'{componentPortion = ComponentPortion},
	TrParms = (State#state.parms)#'TR-UNI'{userData = NewUserData},
	wait_for_uni_components1(State#state{parms = TrParms}).
wait_for_uni_components1(State) ->
	%% TR-UNI request to TSL
	gen_server:cast(State#state.tco, {'TR', 'UNI', request, State#state.parms}),
	%% Dialogue terminated to CHA
	gen_server:cast(State#state.cco, 'dialogue-terminated'),
	%% Free dialogue ID
	{stop, normal, State}.
	
%% reference: Figure A.5 bis/Q.774
%% reference: Figure A.5/Q.774 (sheet 2 of 11)
wait_for_begin_components('no-component', State) ->
	wait_for_begin_components1(State);
wait_for_begin_components({'requested-components', Components}, State) ->
	%% Assemble component portion
	{ok, ComponentPortion} = 'TC':encode('Components', Components),
	%% Assemble TSL user data
	UserData = (State#state.parms)#'TR-BEGIN'.userData,
	NewUserData = UserData#'TR-user-data'{componentPortion = ComponentPortion},
	TrParms = (State#state.parms)#'TR-BEGIN'{userData = NewUserData},
	wait_for_begin_components1(State#state{parms = TrParms}).
wait_for_begin_components1(State) ->
	%% Assign local transaction ID
	TrParms = (State#state.parms)#'TR-BEGIN'{transactionID = tcap_tco_server:new_tid()},
	%% TR-BEGIN request to TSL
	gen_server:cast(State#state.tco, {'TR', 'BEGIN', request, TrParms}),
	{next_state, initiation_sent, State#state{parms = TrParms}}.
	
%% reference: Figure A.5 bis/Q.774
%% reference: Figure A.5/Q.774 (sheet 5 of 11)
wait_cont_components_ir('no-component', State) ->
	wait_cont_components_ir1(State);
wait_cont_components_ir({'requested-components', Components}, State) ->
	%% Assemble component portion
	{ok, ComponentPortion} = 'TC':encode('Components', Components),
	%% Assemble TSL user data
	UserData = (State#state.parms)#'TR-CONTINUE'.userData,
	NewUserData = UserData#'TR-user-data'{componentPortion = ComponentPortion},
	TrParms = (State#state.parms)#'TR-CONTINUE'{userData = NewUserData},
	wait_cont_components_ir1(State#state{parms = TrParms}).
wait_cont_components_ir1(State) ->
	%% TR-CONTINUE request to TSL
	gen_server:cast(State#state.tco, {'TR', 'CONTINUE', request, State#state.parms}),
	{next_state, initiation_sent, State}.
	
%% reference: Figure A.5 bis/Q.774
%% reference: Figure A.5/Q.774 (sheet 9 of 11)
wait_cont_components_active('no-component', State) ->
	wait_cont_components_active1(State);
wait_cont_components_active({'requested-components', Components}, State) ->
	%% Assemble component portion
	{ok, ComponentPortion} = 'TC':encode('Components', Components),
	%% Assemble TSL user data
	UserData = (State#state.parms)#'TR-CONTINUE'.userData,
	NewUserData = UserData#'TR-user-data'{componentPortion = ComponentPortion},
	TrParms = (State#state.parms)#'TR-CONTINUE'{userData = NewUserData},
	wait_cont_components_active1(State#state{parms = TrParms}).
wait_cont_components_active1(State) ->
	%% TR-CONTINUE request to TSL
	gen_server:cast(State#state.tco, {'TR', 'CONTINUE', request, State#state.parms}),
	{next_state, active, State}.
	
%% reference: Figure A.5 bis/Q.774
%% reference: Figure A.5/Q.774 (sheet 5 of 11)
%% reference: Figure A.5/Q.774 (sheet 9 of 11)
wait_for_end_components('no-component', State) ->
	wait_for_end_components1(State);
wait_for_end_components({'requested-components', Components}, State) ->
	%% Assemble component portion
	{ok, ComponentPortion} = 'TC':encode('Components', Components),
	%% Assemble TSL user data
	UserData = (State#state.parms)#'TR-END'.userData,
	NewUserData = UserData#'TR-user-data'{componentPortion = ComponentPortion},
	TrParms = (State#state.parms)#'TR-END'{userData = NewUserData},
	wait_for_end_components1(State#state{parms = TrParms}).
wait_for_end_components1(State) ->
	%% TR-END request to TSL
	gen_server:cast(State#state.tco, {'TR', 'END', request, State#state.parms}),
	%% Dialogue terminated to CHA
	gen_server:cast(State#state.cco, 'dialogue-terminated'),
	%% Free dialogue ID
	{stop, normal, State}.

%% Dialogue portion included? (yes)
extract_uni_dialogue_portion(UserData) when is_record(UserData, 'TR-user-data'),
		UserData#'TR-user-data'.dialoguePortion /= undefined  ->
	%% Dialogue portion correct?
	case 'UnidialoguePDUs':decode('UnidialoguePDU', UserData#'TR-user-data'.dialoguePortion) of
		{unidialoguePDU, AUDT} when is_record(AUDT, 'AUDT-apdu') ->
			%% Is version 1 supported?
			case lists:member(version1, AUDT#'AUDT-apdu'.'protocol-version') of
				true ->
					#'TC-UNI'{appContextName = AUDT#'AUDT-apdu'.'application-context-name',
							userInfo = AUDT#'AUDT-apdu'.'user-information'};
				false ->
					no_version1
			end;
		_ ->
			incorrect_dialogue_portion
	end;
%% Dialogue portion included? (no)
extract_uni_dialogue_portion(_DialoguePortion) ->
	#'TC-UNI'{}.

	
%% Dialogue portion included? (yes)
extract_begin_dialogue_portion(UserData) when is_record(UserData, 'TR-user-data'),
		UserData#'TR-user-data'.dialoguePortion /= undefined ->
	%% Dialogue portion correct?
	case 'DialoguePDUs':decode('DialoguePDU', UserData#'TR-user-data'.dialoguePortion) of
		{dialoguePDU, AARQ} when is_record(AARQ, 'AARQ-apdu') ->
			%% Is version 1 supported?
			case lists:member(version1, AARQ#'AARQ-apdu'.'protocol-version') of
				true ->
					%% Set application context mode
					#'TC-BEGIN'{appContextName = AARQ#'AARQ-apdu'.'application-context-name',
							userInfo = AARQ#'AARQ-apdu'.'user-information'};
				false ->
					no_version1
			end;
		_ ->
			incorrect_dialogue_portion
	end;
%% Dialogue portion included? (no)
extract_begin_dialogue_portion(_DialoguePortion) ->
	#'TC-BEGIN'{}.

extract_dialogue_portion(UserData, undefined) when is_record(UserData, 'TR-user-data'),
		UserData#'TR-user-data'.dialoguePortion /= undefined ->
		%% Dialogue portion included? (yes)  AC mode set? (no)
	abort;
extract_dialogue_portion(UserData, _AppContextName) when not is_record(UserData, 'TR-user-data'),
		UserData#'TR-user-data'.dialoguePortion == undefined ->
		%% Dialogue portion included? (no)  AC mode set? (yes)
	abort;
extract_dialogue_portion(UserData, _AppContextName) when is_record(UserData, 'TR-user-data'),
		UserData#'TR-user-data'.dialoguePortion /= undefined ->
	%% Extract dialogue portion
	%{'EXTERNAL', {syntax,{0,0,17,773,1,1,1}}, _, DlgPDU} = UserData#'TR-user-data'.dialoguePortion,
	% some implementations seem to be broken and not send the 'symtax' part?!?
	{'EXTERNAL', _, _, DlgPDU} = UserData#'TR-user-data'.dialoguePortion,
	case 'DialoguePDUs':decode('DialoguePDU', DlgPDU) of
		{ok, {dialogueResponse, AARE}} when is_record(AARE, 'AARE-apdu') ->
			AARE;	%% Dialogue portion correct? (yes)
		_ ->
			abort	%% Dialogue portion correct? (no)
	end.


%% handle any other message
handle_info(Info, StateName, State) ->
	error_logger:format("dialogue_fsm (~w) received unexpected message: ~w~n", [Info]),
	{next_state, StateName, State}.

%% handle an event sent using gen:fsm_send_all_state_event/2
handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

%% handle an event sent using gen_fsm:sync_send_all_state_event/2,3
handle_sync_event(get_cco_pid, From, StateName, StateData)  ->
	CCO = StateData#state.cco,
	{reply, CCO, StateName, StateData};
handle_sync_event(_Event, _From, StateName, StateData)  ->
	{next_state, StateName, StateData}.

%% handle a shutdown request
terminate(_Reason, _StateName, State) when State#state.supid == undefined ->
	%% we were started by TSM, no worries	
	ets:delete(tcap_dha, State#state.did),
	ok;
terminate(_Reason, _StateName, State) ->
	%% signal TCO so he can reap the ChildSpec of our supervisor
	ets:delete(tcap_dha, State#state.did),
	gen_server:cast(State#state.tco, {'dha-stopped', State#state.supid}).

%% handle updating state data due to a code replacement
code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.


% front-end function called by tcap_user to get CCO for given DHA
get_cco_pid(DHA) ->
	gen_fsm:sync_send_all_state_event(DHA, get_cco_pid).


% Wrap encoded DialoguePortion in EXTERNAL ASN.1 data type
dialogue_ext(undefined) ->
	asn1_NOVALUE;
dialogue_ext(asn1_NOVALUE) ->
	asn1_NOVALUE;
dialogue_ext(DlgEnc) ->
	#'EXTERNAL'{'direct-reference' = {0,0,17,773,1,1,1},
		    'indirect-reference' = asn1_NOVALUE,
		    'encoding' = {'single-ASN1-type', DlgEnc}}.
