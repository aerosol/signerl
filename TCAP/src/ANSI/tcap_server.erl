%%% $Id: tcap_server.erl,v 1.5 2005/08/04 09:33:17 vances Exp $
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
%%% @doc ANSI TCAP service.
%%%
%%% @private
%%%


         
-module(tcap_server).
-copyright('Copyright (c) 2003-2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision: 1.5 $').

-behaviour(gen_server).

%% call backs needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

%% our published API functions
-export([start_link/1, stop/1]).

-include("TCAPPackage.hrl").


%%----------------------------------------------------------------------
%%  The tcap_server exported API
%%----------------------------------------------------------------------

start_link(SubSystemNumber) when is_integer(SubSystemNumber) ->
	RegName = "tcap" ++ "ssn" ++ integer_to_list(SubSystemNumber),
	gen_server:start_link({local, list_to_atom(RegName)}, ?MODULE,
			SubSystemNumber, []).

stop(SubSystemNumber) when is_integer(SubSystemNumber) ->
	RegName = "tcap" ++ "ssn" ++ integer_to_list(SubSystemNumber),
	gen_server:call(list_to_atom(RegName), stop).


%%----------------------------------------------------------------------
%%  The gen_server call backs
%%----------------------------------------------------------------------

%% initialize the server
init([SubSystemNumber]) when is_integer(SubSystemNumber) ->
	NSAPName = "sccp_" ++ "ssn" ++ integer_to_list(SubSystemNumber),
	process_flag(trap_exit, true),
	{ok, list_to_atom(NSAPName)}.

%% shutdown the sccp server
handle_call(stop, _From, State) ->
	{stop, shutdown, State};

%% unrecognized calls
handle_call(Other, From, State) ->
	error_logger:error_report([{unknown_call, Other}, {from, From}]),
	{noreply, State}.

%%%
%%% service primitives received from the TC-User
%%%
handle_cast({'TC', 'UNIDIRECTIONAL', request, Parameters}, State) ->
	{noreply, State};
handle_cast({'TC', 'QUERY-W', request, Parameters}, State) ->
	OTID = new_transaction_id(),
	FsmName = list_to_atom("tr_" ++ integer_to_list(OTID)),
	FsmArgs = [SccpParms, TransactionPDU],
	FsmOptions = [],
	StartArgs = [{local, FsmName}, queryWithPerm_fsm, FsmArgs, FsmOptions],
	StartFunc = {gen_fsm, start_link, StartArgs},
	supervisor:start_child(tcap_sup, {FsmName, StartFunc, transient, worker,
			[queryWithPerm_fsm]}),
	{noreply, State};
handle_cast({'TC', 'QUERY-WO', request, Parameters}, State) ->
	{noreply, State};
handle_cast({'TC', 'CONVERSATION-W', request, Parameters}, State) ->
	{noreply, State};
handle_cast({'TC', 'CONVERSATION-WO', request, Parameters}, State) ->
	{noreply, State};
handle_cast({'TC', 'RESPONSE', request, Parameters}, State) ->
	{noreply, State};
handle_cast({'TC', 'ABORT', request, Parameters}, State) ->
	{noreply, State}.
	
%%%
%%% service primitives received from the SCCP service
%%%
handle_cast({'N', 'UNITDATA', indication, {CalledParty, CallingParty,
			QualityOfServiceParameterSet, NPDU}}, NSAP) ->
	SccpParms = {CalledParty, CallingParty, QualityOfServiceParameterSet},
	%%
	%% Transaction Portion
	%% reference T1.114.2 3.1
	%%
	{ok, Package} = 'TCAPPackage':decode('PackageType', NPDU),
	case Package of
		%%
		%% Unidirectional
		%%
		%% Sends information in one direction only with no reply
		%% expected.  No TCAP Transaction is established.
		%%
		{unidirectional, UniTransactionPDU} ->
			error_logger:error_report([{unhandled_primitive, unidirectional},
					{caller, Caller}, {called, Called}, {package, Package}]),
			{noreply, NSAP};
		%%
		%% Query with permission
		%%
		%% Initiates a TCAP transaction and informs the destination 
		%% it may end the TCAP transaction.
		%%
		{queryWithPerm, TransactionPDU} ->
			OTID = new_transaction_id(),
			FsmName = list_to_atom("tcap_" ++ integer_to_list(OTID)),
			FsmArgs = [SccpParms, TransactionPDU],
			FsmOptions = [],
			StartArgs = [{local, FsmName}, queryWithPerm_fsm, FsmArgs, FsmOptions],
			StartFunc = {gen_fsm, start_link, StartArgs},
			supervisor:start_child(tcap_sup, {FsmName, StartFunc, transient, worker,
					[queryWithPerm_fsm]}),
			{noreply, NSAP};
		%%
		%% Query without permission
		%%
		%% Initiates a TCAP transaction and informs the destination 
		%% it may not end the TCAP transaction.
		%%
		{queryWithoutPerm, TransactionPDU} ->
			error_logger:error_report([{unhandled_primitive, queryWithoutPerm},
					{caller, Caller}, {called, Called}, {package, Package}]),
			{noreply, NSAP};
		%%
		%% Response
		%%
		%% Ends a TCAP transaction
		%%
		{response, TransactionPDU} ->
			{noreply, NSAP};
		%%
		%% Conversation with permission
		%%
		%% Continues a TCAP transaction and informs the destination 
		%% that it may end the TCAP transaction.  
		%%
		{conversationWithPerm, TransactionPDU} ->
			error_logger:error_report([{unhandled_primitive, conversationWithPerm},
					{caller, Caller}, {called, Called}, {package, Package}]),
			{noreply, NSAP};
		%%
		%% Conversation without permission
		%%
		%% Continues a TCAP transaction and informs the destination 
		%% that it may not end the TCAP transaction.  
		%%
		{conversationWithoutPerm, TransactionPDU} ->
			error_logger:error_report([{unhandled_primitive, conversationWithoutPerm},
					{caller, Caller}, {called, Called}, {package, Package}]),
			{noreply, NSAP};
		%%
		%% Abort
		%%
		%% Informs the destination that the sender has terminated
		%% the transaction without sending any pending components.
		%%
		{abort, Abort} ->
			error_logger:error_report([{unhandled_primitive, abaort},
					{caller, Caller}, {called, Called}, {package, Package}]),
			{noreply, NSAP}
	end.

%% unrecognized casts
handle_cast(Other, State) ->
	error_logger:error_report([{unknown_cast, Other}]),
	{noreply, State}.


% trapped exit signals
handle_info({'EXIT', Pid, Reason}, State) ->
	{stop, Reason, State};

% unknown messages
handle_info(Unknown, State) ->
	error_logger:error_msg("Received unknown message: ~p~n", [Unknown]),
	{noreply, State}.

% someone wants us to shutdown and cleanup
terminate(Reason, State) -> ok.

% upgrading the running code
code_change(_, _, _) -> ok.

%%%
%%% internal functions
%%%

%% get the next originating transaction id from the global counter
new_transaction_id() ->
	ets:update_counter(tcap, transactionID, {2, 1, 16#ffffffff, 0}).
