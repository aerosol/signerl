%%% $Id: tcap_tsm_fsm.erl,v 1.3 2005/08/04 09:33:17 vances Exp $
%%%---------------------------------------------------------------------
%%% @copyright 2004-2005 Motivity Telecom, 2010-2011 Harald Welte
%%% @author Vance Shipley <vances@motivity.ca>, Harald Welte <laforge@gnumonks.org>
%%% @end
%%%
%%% Copyright (c) 2004-2005, Motivity Telecom
%%% Copyright (c) 2010-2011, Harald Welte
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
%%% @doc Transaction State Machine (TCM) functional block within the
%%% 		transaction sub-layer of ITU TCAP.
%%%
%%% @reference ITU-T Q.774 (06/97) Annex A Transaction capabilities SDLs
%%%
%%% @private
%%%

-module(tcap_tsm_fsm).
-copyright('Copyright (c) 2004-2005 Motivity Telecom Inc., 2010-2011 Harald Welte').
-author('vances@motivity.ca, laforge@gnumonks.org').
-vsn('$Revision: 1.3 $').

-behaviour(gen_fsm).

%% call backs needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
		terminate/3, code_change/4]).

%% transaction_fsm state callbacks 
-export([idle/2, initiation_sent/2, initiation_received/2, active/2]).

%% record definitions for TR-User primitives
-include("tcap.hrl").
%% record definitions for N-User primitives
-include("sccp.hrl").
%% record definitions for TCAP messages
-include("TCAPMessages.hrl").

%% the transaction_fsm state data
-record(state, {nsap, usap, tco, dha_sup, supref, localTID, remoteTID,
		local_address, remote_address}).

%%----------------------------------------------------------------------
%%  The gen_fsm call backs
%%----------------------------------------------------------------------

%% initialize the server
init([NsapFun, USAP, TID, SupRef, TCO]) ->
	%% store our process identifier in the global transaction ID table
	io:format("Inserting {~p, ~p} into tcap_transactions~n", [TID, self()]),
	ets:insert(tcap_transaction, {TID, self()}),
	process_flag(trap_exit, true),
	{ok, idle, #state{nsap = NsapFun, usap = USAP, localTID = TID,
			supref = SupRef, tco = TCO}}.

%%%
%%% idle state handler
%%% 

%% started by remote
%% reference: Figure A.4/Q.774 (sheet 1 of 5)
idle({'BEGIN', received, SccpParms}, State)
		when is_record(SccpParms, 'N-UNITDATA') ->
	%% Store remote address and remote TID
	NewState = State#state{remote_address = SccpParms#'N-UNITDATA'.callingAddress,
			remoteTID = (SccpParms#'N-UNITDATA'.userData)#'Begin'.otid},
	{ok, Begin} = 'TR':decode('TCMessage', SccpParms#'N-UNITDATA'.userData),
	QOS = {SccpParms#'N-UNITDATA'.sequenceControl, SccpParms#'N-UNITDATA'.returnOption},
	UserData = #'TR-user-data'{dialoguePortion = Begin#'Begin'.dialoguePortion,
			componentPortion = Begin#'Begin'.components},
	TrParms = #'TR-BEGIN'{qos = QOS,
			destAddress = SccpParms#'N-UNITDATA'.calledAddress,
			origAddress = SccpParms#'N-UNITDATA'.callingAddress,
			transactionID = State#state.localTID,
			userData = UserData},
	%% TR-BEGIN CSL <- TSL
	gen_fsm:send_event(resolve_dha(NewState), {'TR', 'BEGIN', indication, TrParms}),
	{next_state, initiation_received, NewState};

%% started by TR-User
%% reference: Figure A.4/Q.774 (sheet 1 of 5)
idle({'BEGIN', transaction, BeginParms}, State)
		when is_record(BeginParms, 'TR-BEGIN') ->
	%% Store local address
	%% NOTE - This may be provided by TC-user or be implicitly associated with
	%%        the access point at which the N-UNITDATA primitive is issued. 
	NewState = State#state{local_address = BeginParms#'TR-BEGIN'.origAddress},
	TrUserData = process_undefined (BeginParms#'TR-BEGIN'.userData),
	DialoguePortion = TrUserData#'TR-user-data'.dialoguePortion,
	ComponentPortion = TrUserData#'TR-user-data'.componentPortion,
	Otid = State#state.localTID,
	Begin = #'Begin'{otid = <<Otid:32/big>>, dialoguePortion = DialoguePortion,
			components = ComponentPortion},
	%% Assemble TR-portion of BEGIN message
	io:format("Trying to encode ~p~n", [Begin]),
	{ok, TPDU} = 'TR':encode('TCMessage', {'begin', Begin}),
	{SequenceControl, ReturnOption} = qos_from_tr_prim(BeginParms),
	SccpParms = #'N-UNITDATA'{calledAddress = BeginParms#'TR-BEGIN'.destAddress,
			callingAddress = BeginParms#'TR-BEGIN'.origAddress,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			importance = none, userData = TPDU},
	%% N-UNITDATA request TSL -> SCCP
	send_to_nsap(State, {'N', 'UNITDATA', request, SccpParms}),
	{next_state, initiation_sent, NewState}.


%%%
%%% initiation_received state handler
%%%
%%% reference: Figure A.4/Q.774 (sheet 2 of 5)

%% Continue from TR-User
initiation_received({'CONTINUE', transaction, ContParms}, State) 
		when is_record(ContParms, 'TR-CONTINUE') ->
	%% Store new local address if it is provided by User
	case ContParms#'TR-CONTINUE'.origAddress of
		undefined ->
			NewState = State;
		NewAddress ->
			NewState = State#state{local_address = NewAddress}
	end,
	TrUserData = process_undefined(ContParms#'TR-CONTINUE'.userData),
	DialoguePortion = TrUserData#'TR-user-data'.dialoguePortion,
	ComponentPortion = TrUserData#'TR-user-data'.componentPortion,
	Otid = State#state.localTID,
	Dtid = State#state.remoteTID,
	Continue = #'Continue'{otid = <<Otid:32/big>>, dtid = <<Dtid:32/big>>,
				dialoguePortion = DialoguePortion, components = ComponentPortion},
	%% Assemble TR-portion of CONTINUE message
	{ok, TPDU} = 'TR':encode('TCMessage', {continue, Continue}),
	{SequenceControl, ReturnOption} = qos_from_tr_prim(ContParms),
	SccpParms = #'N-UNITDATA'{calledAddress = State#state.remote_address,
			callingAddress = NewState#state.local_address,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			importance = none, userData = TPDU},
	%% N-UNITDATA request TSL -> SCCP
	send_to_nsap(State, {'N', 'UNITDATA', request, SccpParms}),
	{next_state, active, NewState};

%% End from TR-User (prearranged)
initiation_received({'END', transaction, EndParms}, State)
		when is_record(EndParms, 'TR-END'),
		EndParms#'TR-END'.termination == prearranged  ->
	{stop, normal, State};
%% End from TR-User (not prearranged)
initiation_received({'END', transaction, EndParms}, State)
		when is_record(EndParms, 'TR-END') ->
	TrUserData = process_undefined(EndParms#'TR-END'.userData),
	DialoguePortion = TrUserData#'TR-user-data'.dialoguePortion,
	ComponentPortion = TrUserData#'TR-user-data'.componentPortion,
	End = #'End'{dialoguePortion = DialoguePortion, components = ComponentPortion},
	%% Assemble TR-portion of END message
	{ok, TPDU} = 'TR':encode('TCMessage', {'end', End}),
	{SequenceControl, ReturnOption} = qos_from_tr_prim(EndParms),
	SccpParms = #'N-UNITDATA'{calledAddress = State#state.remote_address,
			callingAddress = State#state.local_address,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			importance = none, userData = TPDU},
	%% N-UNITDATA request TSL -> SCCP
	send_to_nsap(State, {'N', 'UNITDATA', request, SccpParms}),
	{stop, normal, State};

%% Abort from TR-User
initiation_received({'ABORT', transaction, AbortParms}, State)
		when is_record(AbortParms, 'TR-U-ABORT') ->
	TrUserData = process_undefined(AbortParms#'TR-U-ABORT'.userData),
	Cause = TrUserData#'TR-user-data'.dialoguePortion,
	Abort = #'Abort'{reason = {'u-abortCause', Cause}},
	%% Assemble TR-portion of ABORT message
	{ok, TPDU} = 'TR':encode('TCMessage', {abort, Abort}),
	{SequenceControl, ReturnOption} = qos_from_tr_prim(AbortParms),
	SccpParms = #'N-UNITDATA'{calledAddress = State#state.remote_address,
			callingAddress = State#state.local_address,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			importance = none, userData = TPDU},
	%% N-UNITDATA request TSL -> SCCP
	send_to_nsap(State, {'N', 'UNITDATA', request, SccpParms}),
	{stop, normal, State}.
	

%%%
%%% initiation_sent state handler
%%%
%%% reference: Figure A.4/Q.774 (sheet 2 of 5)

%% Continue from remote
initiation_sent({'CONTINUE', received, SccpParms}, State)
		when is_record(SccpParms, 'N-UNITDATA') ->
	%% Store remote address and remote TID
	Continue = SccpParms#'N-UNITDATA'.userData,
	OTID = Continue#'Continue'.otid,
	NewState = State#state{ remote_address
			= SccpParms#'N-UNITDATA'.callingAddress, remoteTID = OTID},
	QOS = {SccpParms#'N-UNITDATA'.sequenceControl,
			SccpParms#'N-UNITDATA'.returnOption},
	UserData = #'TR-user-data'{dialoguePortion = Continue#'Continue'.dialoguePortion,
				   componentPortion = Continue#'Continue'.components},
	TrParms = #'TR-CONTINUE'{qos = QOS,
			transactionID = State#state.localTID,
			userData = UserData},
	gen_fsm:send_event(resolve_dha(NewState), {'TR', 'CONTINUE', indication, TrParms}),
	{next_state, active, NewState};

%% End from remote
initiation_sent({'END', received, SccpParms}, State)
		when is_record(SccpParms, 'N-UNITDATA') ->
	QOS = {SccpParms#'N-UNITDATA'.sequenceControl,
			SccpParms#'N-UNITDATA'.returnOption},
	End = SccpParms#'N-UNITDATA'.userData,
	UserData = #'TR-user-data'{dialoguePortion = End#'End'.dialoguePortion,
			componentPortion = End#'End'.components},
	TrParms = #'TR-END'{qos = QOS,
			transactionID = State#state.localTID,
			userData = UserData},
	gen_fsm:send_event(resolve_dha(State), {'TR', 'END', indication, TrParms}),
	{stop, normal, State};

%% Abort from remote
initiation_sent({'ABORT', received, SccpParms}, State)
		when is_record(SccpParms, 'N-UNITDATA') ->
	QOS = {SccpParms#'N-UNITDATA'.sequenceControl,
			SccpParms#'N-UNITDATA'.returnOption},
	Abort = SccpParms#'N-UNITDATA'.userData,
	%% TR-U-ABORT?
	case Abort#'Abort'.reason of
		{'p-abortCause', Cause} ->
			TrParms = #'TR-P-ABORT'{qos = QOS,
					transactionID = State#state.localTID,
					pAbort = Cause},
			gen_fsm:send_event(resolve_dha(State), {'TR', 'P-ABORT', indication, TrParms});
		{'u-abortCause', Cause} ->
			UserData = #'TR-user-data'{dialoguePortion = Cause},
			TrParms = #'TR-U-ABORT'{qos = QOS,
					transactionID = State#state.localTID,
					userData = UserData},
			gen_fsm:send_event(resolve_dha(State), {'TR', 'U-ABORT', indication, TrParms})
	end,
	{stop, normal, State};

%% Local Abort 
initiation_sent({'local-abort', received, Cause}, State) ->
	TrParms = #'TR-P-ABORT'{pAbort = Cause},
	gen_fsm:send_event(resolve_dha(State), {'TR', 'P-ABORT', indication, TrParms}),
	{stop, normal, State};

%% End from TR-User
initiation_sent({'END', transaction, EndParms}, State)
		when is_record(EndParms, 'TR-END') ->
	{stop, normal, State};

%% Abort from TR-User
initiation_sent({'ABORT', transaction, AbortParms}, State)
		when is_record(AbortParms, 'TR-U-ABORT') ->
	%% Purely local action
	{stop, normal, State}.
	

%%%
%%% active state handler
%%%
%%% reference: Figure A.4/Q.774 (sheet 2 of 5)

%% Continue received from remote
active({'CONTINUE', received, SccpParms}, State)
		when is_record(SccpParms, 'N-UNITDATA') ->
	Continue = SccpParms#'N-UNITDATA'.userData,
	QOS = {SccpParms#'N-UNITDATA'.sequenceControl,
			SccpParms#'N-UNITDATA'.returnOption},
	UserData = #'TR-user-data'{dialoguePortion = Continue#'Continue'.dialoguePortion,
			componentPortion = Continue#'Continue'.components},
	TrParms = #'TR-CONTINUE'{qos = QOS,
			transactionID = State#state.localTID,
			userData = UserData},
	%% TR-CONTINUE indication CSL <- TSL
	gen_fsm:send_event(resolve_dha(State), {'TR', 'CONTINUE', indication, TrParms}),
	{next_state, active, State};


%% Continue from TR-User
active({'CONTINUE', transaction, ContParms}, State)
		when is_record(ContParms, 'TR-CONTINUE') ->
	TrUserData = process_undefined(ContParms#'TR-CONTINUE'.userData),
	DialoguePortion = TrUserData#'TR-user-data'.dialoguePortion,
	ComponentPortion = TrUserData#'TR-user-data'.componentPortion,
	Otid = State#state.localTID,
	Dtid = State#state.remoteTID,
	io:format("OTID ~p, DTID ~p~n", [Otid, Dtid]),
	Continue = #'Continue'{otid = <<Otid:32/big>>, dtid = <<Dtid:32/big>>,
				dialoguePortion = DialoguePortion, components = ComponentPortion},
	%% Assemble TR-portion of CONTINUE message
	{ok, TPDU} = 'TR':encode('TCMessage', {continue, Continue}),
	{SequenceControl, ReturnOption} = qos_from_tr_prim(ContParms),
	SccpParms = #'N-UNITDATA'{calledAddress = State#state.remote_address,
			callingAddress = State#state.local_address,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			importance = none, userData = TPDU},
	%% N-UNITDATA request TSL -> SCCP
	send_to_nsap(State, {'N', 'UNITDATA', request, SccpParms}),
	{next_state, active, State};

%% End from remote
active({'END', received, SccpParms}, State)
		when is_record(SccpParms, 'N-UNITDATA') ->
	QOS = {SccpParms#'N-UNITDATA'.sequenceControl,
			SccpParms#'N-UNITDATA'.returnOption},
	End = SccpParms#'N-UNITDATA'.userData,
	UserData = #'TR-user-data'{dialoguePortion = End#'End'.dialoguePortion,
			componentPortion = End#'End'.components},
	TrParms = #'TR-END'{qos = QOS,
			transactionID = State#state.localTID,
			userData = UserData},
	%% TR-END indication CSL <- TSL
	gen_fsm:send_event(resolve_dha(State), {'TR', 'END', indication, TrParms}),
	{stop, normal, State};

%% End from TR-User (prearranged)
active({'END', transaction, EndParms}, State)
		when is_record(EndParms, 'TR-END'),
		EndParms#'TR-END'.termination == prearranged  ->
	{stop, normal, State};
%% End from TR-User (not prearranged)
active({'END', transaction, EndParms}, State)
		when is_record(EndParms, 'TR-END') ->
	TrUserData = process_undefined(EndParms#'TR-END'.userData),
	DialoguePortion = TrUserData#'TR-user-data'.dialoguePortion,
	ComponentPortion = TrUserData#'TR-user-data'.componentPortion,
	End = #'End'{dialoguePortion = DialoguePortion, components = ComponentPortion},
	%% Assemble TR-portion of END message
	{ok, TPDU} = 'TR':encode('TCMessage', {'end', End}),
	{SequenceControl, ReturnOption} = qos_from_tr_prim(EndParms),
	SccpParms = #'N-UNITDATA'{calledAddress = State#state.remote_address,
			callingAddress = State#state.local_address,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			importance = none, userData = TPDU},
	send_to_nsap(State, {'N', 'UNITDATA', request, SccpParms}),
	{stop, normal, State};

%% Abort received from remote
active({'ABORT', received, SccpParms}, State)
		when is_record(SccpParms, 'N-UNITDATA') ->
	QOS = {SccpParms#'N-UNITDATA'.sequenceControl,
			SccpParms#'N-UNITDATA'.returnOption},
	Abort = SccpParms#'N-UNITDATA'.userData,
	%% TR-U-ABORT?
	case Abort#'Abort'.reason of
		{'p-abortCause', Cause} ->  % No
			TrParms = #'TR-P-ABORT'{qos = QOS,
					transactionID = State#state.localTID,
					pAbort = Cause},
			%% TR-P-ABORT indication CSL <- TSL
			gen_fsm:send_event(resolve_dha(State), {'TR', 'P-ABORT', indication, TrParms});
		{'u-abortCause', Cause} ->  % Yes
			UserData = #'TR-user-data'{dialoguePortion = Cause},
			TrParms = #'TR-U-ABORT'{qos = QOS,
					transactionID = State#state.localTID,
					userData = UserData},
			%% TR-U-ABORT indication CSL <- TSL
			gen_fsm:send_event(resolve_dha(State), {'TR', 'U-ABORT', indication, TrParms})
	end,
	{stop, normal, State};

%% Local Abort 
active({'local-abort', received, Cause}, State) ->
	TrParms = #'TR-P-ABORT'{qos = {false, false},
			transactionID = State#state.localTID, pAbort= Cause},
	%% TR-P-ABORT indication CSL <- TSL
	gen_fsm:send_event(resolve_dha(State), {'TR', 'P-ABORT', indication, TrParms}),
	{stop, normal, State};

%% Abort from TR-User
active({'ABORT', transaction, AbortParms}, State) 
		when is_record(AbortParms, 'TR-U-ABORT') ->
	TrUserData = process_undefined(AbortParms#'TR-U-ABORT'.userData),
	Cause = TrUserData#'TR-user-data'.dialoguePortion,
	Abort = #'Abort'{reason = {'u-abortCause', Cause}},
	%% Assemble TR-portion of ABORT message
	{ok, TPDU} = 'TR':encode('TCMessage', {abort, Abort}),
	{SequenceControl, ReturnOption} = qos_from_tr_prim(AbortParms),
	SccpParms = #'N-UNITDATA'{calledAddress = State#state.remote_address,
			callingAddress = State#state.local_address,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			importance = none, userData = TPDU},
	%% N-UNITDATA request TSL -> SCCP
	send_to_nsap(State, {'N', 'UNITDATA', request, SccpParms}),
	{stop, normal, State}.


%% handle an event sent using gen_fsm:send_all_state_event/2
handle_event(Event, StateName, State) -> 
	error_logger:format("transaction_fsm (~w) received unexpected message: ~w~n", [Event]),
	{next_state, StateName, State}.

%% handle an event sent using gen_fsm:sync_send_all_state_event/2,3
handle_sync_event(Event, _From, StateName, State) ->
	error_logger:format("transaction_fsm (~w) received unexpected message: ~w~n", [Event]),
	{next_state, StateName, State}.

%% handle any other message
handle_info(Info, StateName, State) ->
	error_logger:format("transaction_fsm (~w) received unexpected message: ~w~n", [Info]),
	{next_state, StateName, State}.

%% handle a shutdown request
terminate(_Reason, _StateName, State) ->
	io:format("Deleting {~p, ~p} from tcap_transactions~n", [ State#state.localTID, self()]),
	ets:delete(tcap_transaction, State#state.localTID),
	%% signal TCO that we are stopping
	gen_server:cast(State#state.tco, {'tsm-stopped', State#state.supref}).

%% handle updating state data due to a code replacement
code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.


%% internal helper function to send a primitive through the NSAP
send_to_nsap(State, P) when is_record(State, state) ->
	SendFun = State#state.nsap,
	SendFun(P).

process_undefined(U = #'TR-user-data'{dialoguePortion = undefined}) ->
	U#'TR-user-data'{dialoguePortion = asn1_NOVALUE};
process_undefined(U = #'TR-user-data'{}) ->
	U.

resolve_dha(DlgId) when is_integer(DlgId) ->
	[{DlgId, DHA}] = ets:lookup(tcap_dha, DlgId),
	DHA;
resolve_dha(#state{localTID = TID}) ->
	resolve_dha(TID).

% QoS from TR-* primitive to {SequenceControl, ReturnOpt}
qos_from_tr_qos(undefined) ->
	{false, true};
qos_from_tr_qos({Seq, Ret}) ->
	{Seq, Ret}.

qos_from_tr_prim(#'TR-CONTINUE'{qos=Qos}) ->
	qos_from_tr_qos(Qos);
qos_from_tr_prim(#'TR-BEGIN'{qos=Qos}) ->
	qos_from_tr_qos(Qos);
qos_from_tr_prim(#'TR-END'{qos=Qos}) ->
	qos_from_tr_qos(Qos);
qos_from_tr_prim(#'TR-U-ABORT'{qos=Qos}) ->
	qos_from_tr_qos(Qos).
