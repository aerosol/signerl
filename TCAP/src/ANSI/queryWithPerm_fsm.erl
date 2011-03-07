%%% $Id: queryWithPerm_fsm.erl,v 1.5 2005/08/04 09:33:17 vances Exp $
%%%---------------------------------------------------------------------
%%% @copyright 2004-2005 Motivity Telecom Inc.
%%% @author Vance Shipley <vances@motivity.ca> [http://www.motivity.ca]
%%% @end
%%%
%%% Copyright Motivity Telecom Inc. 2003-2005
%%%
%%% All rights reserved. No part of this computer program(s) may be
%%% used, reproduced, stored in any retrieval system, or transmitted,
%%% in any form or by any means, electronic, mechanical, photocopying,
%%% recording, or otherwise without prior written permission of
%%% Motivity Telecom Inc.
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
