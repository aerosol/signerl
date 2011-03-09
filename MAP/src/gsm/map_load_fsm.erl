%%% $Id: map_load_fsm.erl,v 1.1 2005/02/13 09:45:06 vances Exp $
%%%---------------------------------------------------------------------
%%% @copyright 2005 Motivity Telecom Inc.
%%% @author Vance Shipley <vances@motivity.ca>
%%% @end
%%%
%%% Copyright 2005 Motivity Telecom Inc.
%%%
%%% All rights reserved. No part of this computer program(s) may be
%%% used, reproduced, stored in any retrieval system, or transmitted,
%%% in any form or by any means, electronic, mechanical, photocopying,
%%% recording, or otherwise without prior written permission of
%%% Motivity Telecom Inc.
%%%---------------------------------------------------------------------
%%%
%%% @doc GSM MAP Load Control.
%%%
%%% @private
%%%
%%% @reference 3GPP TS 29.02 Figure 15.6/5: Process Load_Ctrl
%%%
-module(map_dsm_fsm).
-copyright('Copyright (c) 2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-behaviour(gen_fsm).

%% export the gen_fsm state handler call backs
-export([idle/2, congested/2]).

%% export the gen_fsm common call backs
-export([init/1, handle_event/3, handle_sync_event/4,
		handle_info/3, terminate/3, code_change/4]).

%% include record definitions for TC service primitives
-include("tcap.hrl").
%% include record definitions for MAP service primitives
-include("map.hrl").

%% StateData record definition
-record(state, {}).

%%----------------------------------------------------------------------
%%  The gen_fsm call backs
%%----------------------------------------------------------------------

%% @hidden
init(_Args) ->
	process_flag(trap_exit, true),
	{ok, idle, StateData}.
                
%% @spec(Event, StateData) -> Result
%%
%% @doc State handler for the idle state.
%%
%% @hidden
%%
idle({DSM, 'Check_Load', _AC}, StateData) ->
	gen_fsm:send_event(DSM, 'Load_OK'),
	{next_state, idle, StateData}.

%% @spec(Event, StateData) -> Result
%%
%% @doc State handler for the congested state.
%%
%% @hidden
%%
congested({DSM, 'Check_Load', AC}, StateData) ->
	% AC = Secure_Transport?
		% true
			% compare encapsulated AC priority with load
		% false
			% compare AC priority with load
	% dialogue acceptable?
		% true	
			gen_fsm:send_event(DSM, 'Load_OK'),
		% false
			% gen_fsm:send_event(DSM, 'Overload'),
	{next_state, congested, StateData}.

%% @hidden
terminate(_Reason, _StateName, _StateData) ->
	ok.

%% @hidden
code_change(_OldVersion, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.


%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

