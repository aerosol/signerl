%%% $Id: queryWithPerm_fsm.erl,v 1.5 2005/08/04 09:33:17 vances Exp $
%%%---------------------------------------------------------------------
%%% @copyright 2003-2005 Motivity Telecom
%%% @author Vance Shipley <vances@motivity.ca> [http://www.motivity.ca]
%%% @end
%%%
%%% Copyright (c) 2003-2005, Motivity Telecom
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
%%% @doc ANSI TCAP'Query with Permission' package.
%%%  <p>This module implements an ANSI TCAP 'Query with Permission' package
%%%  type transaction process.  The components within the received 
%%%  transaction PDU are executed sequentially in this process.</p>
%%%
%%%  @reference ANSI T1.114-2000
%%%
%%% @private
%%%


         
-module(queryWithPerm_fsm).
-copyright('Copyright (c) 2003-2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision: 1.5 $').

-behaviour(gen_fsm).

%% call backs needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
	terminate/3, code_change/4]).

%% call backs for gen_fsm states in this module
%% our published API functions
-export([]).

-include("TCAPPackage.hrl").


%%----------------------------------------------------------------------
%%  The gen_server call backs
%%----------------------------------------------------------------------

%% initialize the server
init([SccpParms, TransactionPDU]) ->
	{CalledParty, CallingParty, QualityOfServiceParameterSet} = SccpParms,
	{ok, 
	process_flag(trap_exit, true),
	{ok, StateName, StateData, Timeout}.

%% handle events sent with gen_fsm:send_all_state_event/2
handle_event(Event, StateName, StateData) -> 
	{next_state, StateName, StateData}.

% trapped exit signals
handle_info({'EXIT', Pid, Reason}, State) ->
	{stop, Reason, State}.

%% handle events sent with gen_fsm:sync_send_all_state_event/2,3
handle_sync_event(Event, From, StateName, StateData) ->
	{next_state, StateName, StateData}.

% handle other received messages 
handle_info(Unknown, State) ->
	error_logger:error_msg("Received unknown message: ~p~n", [Unknown]),
	{next_state, StateName, StateData}.

% someone wants us to shutdown and cleanup
terminate(Reason, State) -> ok.

% upgrading the running code
code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.


%%%
%%% The states for the 'Query with Permission' finite state machine
%%%

package_sent(timeout, StateData) ->
	{next_state, StateName, StateData, Timeout};
package_sent(Event, StateData) ->
	{next_state, StateName, StateData, Timeout}.

package_received(timeout, StateData) ->
	{next_state, StateName, StateData, Timeout};
package_received(Event, StateData) ->
	{next_state, StateName, StateData, Timeout}.

%%%
%%% internal functions
%%%
