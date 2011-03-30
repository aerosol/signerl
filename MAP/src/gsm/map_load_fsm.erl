%%% $Id: map_load_fsm.erl,v 1.1 2005/02/13 09:45:06 vances Exp $
%%%---------------------------------------------------------------------
%%% @copyright 2005 Motivity Telecom
%%% @author Vance Shipley <vances@motivity.ca> [http://www.motivity.ca]
%%% @end
%%%
%%% Copyright (c) 2005, Motivity Telecom
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

