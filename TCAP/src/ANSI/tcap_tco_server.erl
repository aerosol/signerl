%%% $Id: tcap_tco_server.erl,v 1.3 2005/08/04 09:33:17 vances Exp $
%%%---------------------------------------------------------------------
%%% @copyright 2004-2005 Motivity Telecom
%%% @author Vance Shipley <vances@motivity.ca> [http://www.motivity.ca]
%%% @end
%%%
%%% Copyright (c) 2004-2005, Motivity Telecom
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
%%% @doc Transaction Coordinator (TCO) functional block within the
%%% 		transaction sub-layer of ANSI TCAP.
%%%
%%% @reference ANSI T1.114.4 Transaction Capabilities Procedures 
%%% @reference ITU-T Q.774 (06/97) Annex A Transaction capabilities SDLs
%%%
%%% @private
%%%

         
-module(tcap_tco_server).
-copyright('Copyright (c) 2003-2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision: 1.3 $').

-behaviour(gen_server).

%% call backs needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

%% our published API functions
-export([new_tid/0]).

-include("TCAPMessages.hrl").
-include("tcap.hrl").
-include("sccp.hrl").

-record(state, {supervisor, nsap, usap}).

%%----------------------------------------------------------------------
%%  The gen_server call backs
%%----------------------------------------------------------------------

%% initialize the server
init([Supervisor, NSAP, USAP]) ->
	% NSAP = "sccp_" ++ "ssn" ++ integer_to_list(SubSystemNumber),
	process_flag(trap_exit, true),
	{ok, #state{supervisor = Supervisor, nsap = NSAP, usap = USAP}}.

%% shutdown the server
handle_call(stop, _From, State) ->
	{stop, shutdown, ok, State};

%% unrecognized calls
handle_call(Other, From, State) ->
	error_logger:error_report([{unknown_call, Other}, {from, From}]),
	{noreply, State}.

%%%
%%% service primitive indications from the network layer
%%%
%%% reference: Figure A.3/Q.774 (sheet 1 of 4)
%%%
handle_cast({'N', 'UNITDATA', indication, UdataParms}, State) 
		when is_record(UdataParms, 'N-UNITDATA') ->
	case 'TR':decode('TCMessage', UdataParms#'N-UNITDATA'.userData) of
		{ok, {unidirectional, TPDU}} ->
			case 'TR':decode('Unidirectional', TPDU) of
				{ok, Unidirectional} ->
					%% Create a Dialogue Handler (DHA) 
					DialogueID = new_tid(),
					SupId = list_to_atom("dha_" ++ integer_to_list(DialogueID)),
					{ok, {M, F, A, Mods} = application:get_env(start_dha),
					StartFunc = {M, F, A ++ [{State#state.nsap, State#state.usap, DialogueID, self(), SupId}]},
					ChildSpec = {SupId, StartFunc, temporary, 10000, worker, Mods},
					{ok, DHA} = supervisor:start_child(State#state.supervisor, ChildSpec),
					%% TR-UNI indication CSL <- TSL
					UserData = #'TR-user-data'{dialoguePortion = Unidirectional#'Unidirectional'.dialoguePortion,
							componentPortion = Unidirectional#'Unidirectional'.components},
					TrParms = #'TR-UNI'{qos =
							destAddress = UdataParms#'N-UNITDATA'.calledAddress,
							origAddress = UdataParms#'N-UNITDATA'.callingAddress,
							userData = UserData},
					gen_fsm:send_event(DHA, {'TR', 'UNI', indication, TrParms}),
					{noreply, State};
				{error, Reason} ->
					%% Discard received message
					%% reference: Figure A.3/Q/774 (sheet 4 of 4) label (3)
					error_logger:error_report(["Syntax error in received N-UNI",
							{nsap, State#state.nsap}, {error, Reason},
							{caller, UdataParms#'N-UNITDATA'.callingAddress},
							{called, UdataParms#'N-UNITDATA'.calledAddress}]),
					{noreply, State}
			end;
		{ok, {'begin', TPDU}} ->
			case 'TR':decode('Begin', TPDU) of
				{ok, Begin} ->
					%% Assign local transaction ID
					TID = new_tid(),
					ChildName = list_to_atom("tsm_sup_" ++ integer_to_list(TID)),
					{ok, {M, F, A, Mods} = application:get_env(start_tsm),
					StartFunc = {M, F, A ++ [{State#state.nsap, State#state.usap, TID}]},
					ChildSpec = {ChildName, StartFunc, temporary, infinity, supervisor, Mods},
					%% Is TID = no TID?
					%% Note:  The assignment of the ID above just gets the next available
					%%        value and doesn't ensure that it is not in use (unlikely)
					%%        or that there are enough resources available.  The real
					%%        test is in whether the start succeeds.
					case supervisor:start_child(State#state.supervisor, ChildSpec) of
						{ok, TSM} ->
							%% Created a Transaction State Machine (TSM)
							TsmParms = UdataParms#'N-UNITDATA'{userData = Begin},
							%% BEGIN received TSM <- TCO
							gen_fsm:send_event(TSM, {'BEGIN', received, TsmParms});
						_Other ->
							%% TID = no TID
							%% Build ABORT message (P-Abort Cause = Resource Limitation)
							Abort = {abort, #'Abort'{dtid = TPDU#'Begin'.otid,
									reason = {'p-abortCause', resourceLimitation}}},
							NewTPDU = list_to_binary('TCMessage':encode('TCMessage', Abort)),
							SccpParms = #'N-UNITDATA'{calledAddress = UdataParms#'N-UNITDATA'.callingAddress,
									callingAddress = UdataParms#'N-UNITDATA'.calledAddress,
									sequenceControl = false, returnOption = false, importance = none,
									userData = NewTPDU},
							%% TR-UNI request TSL -> SCCP
							gen_fsm:send_event(State#state.nsap, {'N', 'UNITDATA', request, SccpParms}),
							error_logger:error_report(["Unable to create TSM for received N-BEGIN",
									{nsap, State#state.nsap}, {caller, UdataParms#'N-UNITDATA'.callingAddress},
									{called, UdataParms#'N-UNITDATA'.calledAddress}])
					end,
					{noreply, State};
				{error, Reason} ->
%% TODO
					%% is OTID derivable?
					%%    Build ABORT message with appropraite P-Abort Cause value
					%%    N-UNITDATA request TSL -> SCCP
					%% Discard received message
					%% reference: Figure A.3/Q/774 (sheet 4 of 4) label (4)
					error_logger:error_report(["Syntax error in received N-BEGIN", {error, Reason},
									{nsap, State#state.nsap}, {caller, UdataParms#'N-UNITDATA'.callingAddress},
									{called, UdataParms#'N-UNITDATA'.calledAddress}]),
					{noreply, State}
			end;
		{ok, {continue, TPDU}} ->
			case 'TR':decode('Continue', TPDU) of
				{ok, Continue} ->
					%% DTID assigned?
					case catch ets:lookup_element(tcap_transaction, TPDU#'Continue'.dtid, 2) of
						{error, _Reason}  ->
							error_logger:error_report(["DTID not found in received N-CONTINUE",
									{dtid, TPDU#'End'.dtid}, {nsap, State#state.nsap},
									{caller, UdataParms#'N-UNITDATA'.callingAddress},
									{called, UdataParms#'N-UNITDATA'.calledAddress}]),
%% TODO
							%% Build ABORT message with appropriate P-Abort Cause values
							%% N-UNITDATA request TSL -> SCCP
							%% Discard received message
							%% reference: Figure A.3/Q/774 (sheet 4 of 4) label (4)
							{noreply, State};
						TSM ->
							TsmParms = UdataParms#'N-UNITDATA'{userData = Continue},
							%% CONTINUE received TSM <- TCO
							gen_fsm:send_event(TSM, {'CONTINUE', received, TsmParms}),
							{noreply, State}
					end;
				{error, Reason} ->
%% TODO
					%% OTID derivable?
					%% DTID assigned?
					%% Build ABORT message with appropraite P-Abort Cause value
					%% N-UNITDATA request TSL -> SCCP
					%% Local Abort TSM <- TCO
					%% Discard received message
					%% reference: Figure A.3/Q/774 (sheet 4 of 4) label (2)
					error_logger:error_report(["Syntax error in received N-CONTINUE", {error, Reason},
							{nsap, State#state.nsap}, {caller, UdataParms#'N-UNITDATA'.callingAddress},
							{called, UdataParms#'N-UNITDATA'.calledAddress}]),
					{noreply, State}
			end;
		{ok, {'end', TPDU}} ->
			case 'TR':decode('End', TPDU) of
				{ok, End} ->
					%% DTID assigned?
					case catch ets:lookup(tcap_transaction, TPDU#'End'.dtid, 2) of
						{error, _Reason}  ->
							error_logger:error_report(["DTID not found in received N-END",
									{dtid, TPDU#'End'.dtid}, {nsap, State#state.nsap}, 
									{caller, UdataParms#'N-UNITDATA'.callingAddress},
									{called, UdataParms#'N-UNITDATA'.calledAddress}]),
							%% Discard received message
							%% reference: Figure A.3/Q/774 (sheet 4 of 4) label (3)
							{noreply, State};
						TSM ->
							TsmParms = UdataParms#'N-UNITDATA'{userData = End},
							%% END received TSM <- TCO
							gen_fsm:send_event(TSM, {'END', received, TsmParms}),
							{noreply, State}
					end;
				{error, Reason} ->
%% TODO
					%% DTID assigned?
					%%    Local Abort TSM <- TCO
					%% Discard received message
					%% reference: Figure A.3/Q/774 (sheet 4 of 4) label (5)
					error_logger:error_report(["Syntax error in received N-END", {error, Reason},
							{nsap, State#state.nsap}, {caller, UdataParms#'N-UNITDATA'.callingAddress},
							{called, UdataParms#'N-UNITDATA'.calledAddress}]),
					{noreply, State}
			end;
		{ok, {abort, TPDU}} ->
			case 'TR':decode('Abort', TPDU) of
				{ok, Abort} ->
					%% DTID assigned?
					case catch ets:lookup(tcap_transaction, TPDU#'Abort'.dtid, 2) of
						{error, _Reason} ->
							error_logger:error_report(["DTID not found in received N-ABORT",
									{dtid, TPDU#'Abort'.dtid}, {nsap, State#state.nsap},
									{caller, UdataParms#'N-UNITDATA'.callingAddress},
									{called, UdataParms#'N-UNITDATA'.calledAddress}]),
							%% Discard received message
							%% reference: Figure A.3/Q/774 (sheet 4 of 4) label (3)
							{noreply, State};
						TSM ->
							TsmParms = UdataParms#'N-UNITDATA'{userData = Abort},
							%% Abort received TSM <- TCO
							gen_fsm:send_event(TSM, {'ABORT', received, TsmParms}),
							{noreply, State}
					end;
				{error, Reason} ->
%% TODO
					%% DTID assigned?
					%%    Local Abort TSM <- TCO
					%% Discard received message
					%% reference: Figure A.3/Q/774 (sheet 4 of 4) label (5)
					error_logger:error_report(["Syntax error in received N-ABORT", {error, Reason},
							{nsap, State#state.nsap}, {caller, UdataParms#'N-UNITDATA'.callingAddress},
							{called, UdataParms#'N-UNITDATA'.calledAddress}]),
					{noreply, State}
			end;
		{ok, {error, Reason}} ->
%% TODO
			%% Message type unknown
			%% OTID derivable?
			%% DTID assigned?
			%% Build ABORT message with appropraite P-Abort Cause value
			%% N-UNITDATA request TSL -> SCCP
			%% Local Abort TSM <- TCO
			%% Discard received message
			%% reference: Figure A.3/Q/774 (sheet 4 of 4) label (2)
			error_logger:error_report(["Unknown TCMessage received", {error, Reason},
							{nsap, State#state.nsap}, {caller, UdataParms#'N-UNITDATA'.callingAddress},
							{called, UdataParms#'N-UNITDATA'.calledAddress}]),
			{noreply, State}
	end;

handle_cast({'N', 'NOTICE', indication, NoticeParms}, State) ->
	%% Extract the originating transactionID
	case 'TR':decode('TCMessage', NoticeParms#'N-NOTICE'.userData) of
		{ok, {'begin', TPDU}} ->
			case 'TR':decode('Begin', TPDU) of
				{ok, Begin} ->
					TransactionID = Begin#'Begin'.otid;
				_ ->
					TransactionID = undefined
			end;
		{ok, {continue, TPDU}} ->
			case 'TR':decode('Continue', TPDU) of
				{ok, Continue} ->
					TransactionID = Continue#'Continue'.otid;
				_ ->
					TransactionID = undefined
			end;
		_ ->
			TransactionID = undefined
	end,
	%% TR-NOTICE indication CSL <- TSL
	%% reference: Figure A.3/Q.774 (sheet 2 of 4)
	%% The CSL is a null layer for this indication so it becomes
	%% TC-NOTICE indication TCU <- TSL
	%% reference: Figure A.5/Q.774 (sheet 7 of 11)
	%% reference: Figure A.3/Q.774 (sheet 10 of 11)
	TcParms = #'TC-NOTICE'{
			dialogueID = TransactionID,
			origAddress = NoticeParms#'N-NOTICE'.callingAddress,
			destAddress = NoticeParms#'N-NOTICE'.calledAddress,
			reportCause = NoticeParms#'N-NOTICE'.reason},
	gen_fsm:send_event(State#state.usap, {'TC', 'NOTICE', indication, TcParms}),
	{noreply, State};


%%%
%%% service primitive requests from the TR-User
%%% reference: Figure A.3/Q.774 (sheets 2&3 of 4)
handle_cast({'TR', 'UNI', request, UniParms}, State) 
		when is_record(UniParms, 'TR-UNI') ->
	%% Assemble TR-portion of UNI message
	{SequenceControl, ReturnOption, Importance} = UniParms#'TR-UNI'.qos,
	DialoguePortion = (UniParms#'TR-UNI'.userData)#'TR-user-data'.dialoguePortion,
	ComponentPortion = (UniParms#'TR-UNI'.userData)#'TR-user-data'.componentPortion,
	TPDU = 'TR':encode('TCMessage', {unidirectional, #'Unidirectional'{
			dialoguePortion = DialoguePortion, components = ComponentPortion}}),
	SccpParms = #'N-UNITDATA'{calledAddress = UniParms#'TR-UNI'.destAddress,
			callingAddress =  UniParms#'TR-UNI'.origAddress,
			sequenceControl = SequenceControl, returnOption = ReturnOption,
			importance = Importance, userData = TPDU},
	gen_fsm:send_event(State#state.nsap, {'N', 'UNITDATA', request, SccpParms}),
	{noreply, State};
handle_cast({'TR', 'BEGIN', request, BeginParms}, State) 
		when is_record(BeginParms, 'TR-BEGIN') ->
	%% Create a Transaction State Machine (TSM)
	OTID = BeginParms#'TR-BEGIN'.transactionID,
	ChildName = list_to_atom("tsm_" ++ integer_to_list(OTID)),
	{ok, {M, F, A, Mods}} = application:get_env(start_tsm),
	StartFunc = {M, F, A ++ [{State#state.nsap, State#state.usap, OTID, ChildName}]},
	ChildSpec = {ChildName, StartFunc, temporary, infinity, worker, Mods},
	{ok, TSM} = supervisor:start_child(State#state.supervisor, ChildSpec),
	gen_fsm:send_event(TSM, {'BEGIN', transaction, BeginParms}),
	{noreply, State};
handle_cast({'TR', 'CONTINUE', request, ContParms}, State)
		when is_record(ContParms, 'TR-CONTINUE') ->
	TransactionID = ContParms#'TR-CONTINUE'.transactionID,
	TSM  = ets:lookup_element(tcap_transaction, TransactionID, 2),
	gen_fsm:send_event(TSM, {'CONTINUE', transaction, ContParms}),
	{noreply, State};
handle_cast({'TR', 'END', request, EndParms}, State)
		when is_record(EndParms, 'TR-END') ->
	TransactionID = EndParms#'TR-END'.transactionID,
	TSM  = ets:lookup_element(tcap_transaction, TransactionID, 2),
	gen_fsm:send_event(TSM, {'END', transaction, EndParms}),
	{noreply, State};
handle_cast({'TR', 'U-ABORT', request, AbortParms}, State)
		when is_record(AbortParms, 'TR-U-ABORT') ->
	TransactionID = AbortParms#'TR-U-ABORT'.transactionID,
	TSM  = ets:lookup_element(tcap_transaction, TransactionID, 2),
	gen_fsm:send_event(TSM, {'ABORT', transaction, AbortParms}),
	{noreply, State};
	
%%
%% The TSM sends us a message as it's last action so
%% we can remove the supervisor child specification
%%
handle_cast({'tsm-stopped', SupRef}, State) ->
	supervisor:delete_child(State#state.supervisor, SupRef),
	%% reference: Figure A.3/Q/774 (sheet 2 of 4)
	{noreply, State};

%% unrecognized casts
handle_cast(Other, State) ->
	error_logger:error_report([{unknown_cast, Other}]),
	{noreply, State}.


%% trapped exit signals
handle_info({'EXIT', _Pid, Reason}, State) ->
	{stop, Reason, State};

%% unknown messages
handle_info(Unknown, State) ->
	error_logger:error_msg("Received unknown message: ~p~n", [Unknown]),
	{noreply, State}.

%% someone wants us to shutdown and cleanup
terminate(_Reason, _State) -> ok.

%% upgrading the running code
code_change(_, _, _) -> ok.

%%%
%%% internal functions
%%%

%% get the next originating transaction id from the global counter
%%
%% TODO:  we are simply assuming that when the counter rolls over the last 
%%        transaction to have this ID is long gone (4.2 billion IDs)
%%
%% reference: Figure A.3 bis/Q.774
new_tid() ->
	ets:update_counter(tcap_transaction, transactionID, {2, 1, 16#ffffffff, 0}).

