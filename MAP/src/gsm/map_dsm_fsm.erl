%%% $Id: map_dsm_fsm.erl,v 1.2 2005/02/13 09:45:06 vances Exp $
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
%%% @doc GSM MAP Dialogue State Machine (DSM).
%%%
%%% @reference 3GPP TS 29.02 Figure 15.6/3: Process Secure_MAP_DSM
%%%
-module(map_dsm_fsm).
-copyright('Copyright (c) 2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-behaviour(gen_fsm).

%% behaviour modules must export this function
-export([behaviour_info/1]).

%% export the gen_fsm interface
-export([start/3, start/4, start_link/3, start_link/4,
		send_event/2, sync_send_event/2, sync_send_event/3,
		send_all_state_event/2, sync_send_all_state_event/2,
		sync_send_all_state_event/3, reply/2, send_event_after/2,
		start_timer/2, cancel_timer/1]).

%% export the gen_fsm state handler call backs
%-export([statename/2, statename/3]).

%% export the gen_fsm common call backs
-export([init/1, terminate/3, code_change/4]).

%% define what callbacks users must export
%%
%% @hidden
behaviour_info(callbacks) ->
	gen_fsm:behaviour_info(callbacks);
behaviour_info(Other) -> 
	gen_fsm:behaviour_info(Other).

%% include record definitions for TC service primitives
-include_lib("tcap/include/tcap.hrl").
%% include record definitions for MAP service primitives
-include_lib("map/include/map.hrl").

%% StateData record definition
-record(state, {module, ext_statename, ext_statedata,
		applicaton_context, requests = [],
		% @reference 3GPP TS 2902 Figure 15.6/3a: Process Secure_MAP_DSM (sheet 1) DCL
		secure_transport_required, components_present, ac_name_unchanged, 
		encapsulated_ac_name_unchanged, ac_included, ac_supported, 
		invoke_id_active, last_component, operation_exists,
		alternative_name_exists, user_info_included, op_code}).


%%----------------------------------------------------------------------
%%  The gen_fsm API functions
%%----------------------------------------------------------------------

%% @hidden
start(Module, Args, Options) ->
	gen_fsm:start(?MODULE, [Module, Args], Options).

%% @hidden
start(FsmRef, Module, Args, Options) ->
	gen_fsm:start(FsmRef, ?MODULE, [Module, Args], Options).

%% @hidden
start_link(Module, Args, Options) ->
	gen_fsm:start_link(?MODULE, [Module, Args], Options).

%% @hidden
start_link(FsmRef, Module, Args, Options) ->
	gen_fsm:start_link(FsmRef, ?MODULE, [Module, Args], Options).

%% @hidden
send_event(FsmRef, Event) ->
	gen_fsm:send_event(FsmRef, Event).

%% @hidden
sync_send_event(FsmRef, Event) ->
	gen_fsm:sync_send_event(FsmRef, Event).

%% @hidden
sync_send_event(FsmRef, Event, Timeout) ->
	gen_fsm:sync_send_event(FsmRef, Event, Timeout).

%% @hidden
send_all_state_event(FsmRef, Event) ->
	gen_fsm:send_all_state_event(FsmRef, Event).

%% @hidden
sync_send_all_state_event(FsmRef, Event) ->
	gen_fsm:sync_send_all_state_event(FsmRef, Event).

%% @hidden
sync_send_all_state_event(FsmRef, Event, Timeout) ->
	gen_fsm:sync_send_all_state_event(FsmRef, Event, Timeout).

%% @hidden
reply(Caller, Reply) ->
	gen_fsm:reply(Caller, Reply).

%% @hidden
send_event_after(Time, Event) ->
	gen_fsm:send_event_after(Time, Event).

%% @hidden
start_timer(Time, Msg) ->
	gen_fsm:start_timer(Time, Msg).

%% @hidden
cancel_timer(Ref) ->
	gen_fsm:cancel_timer(Ref).


%%----------------------------------------------------------------------
%%  The gen_fsm call backs
%%----------------------------------------------------------------------

%% @hidden
init([Module, Args]) ->
	process_flag(trap_exit, true),
	% initialize user callback module 
	case Module:init(Args) of
		{ok, ExtStateName, ExtStateData} ->
			StateData = #state{module = Module, ext_statename = ExtStateName, ext_statedata = ExtStateData},
			{ok, idle, StateData};
		{ok, ExtStateName, ExtStateData, Timeout} ->
			StateData = #state{module = Module, ext_statename = ExtStateName, ext_statedata = ExtStateData},
			{ok, idle, State, Timeout};
		{stop, Reason} ->
			{stop, Reason};
		ignore ->
			ignore;
		Other ->
			Other
	end.
                
%% @spec(Event, StateData) -> Result
%%
%% @doc State handler for the idle state.
%%
%% @hidden
%%
%
% @reference 3GPP TS 29.002 15.2.1 Behaviour at the initiating side
% @reference 3GPP TS 29.002 Figure 15.6/3a: Process_Secure_MAP_DSM (sheet 1)
%
idle({'MAP', 'OPEN', request, Open} = Event, StateData) ->
	AC = Open#'MAP-OPEN'.application_context_name,
	AC_PDU = Open#'MAP-OPEN'.specific_information,
	% secure transport required?  -- according to the AC and the identity of responder
	case secure_transport_required(AC, AC_PDU) of
		% true
		true ->
			% set AC: Secure_Transport
			% build encapsulated AC PDU
			NewStateData = build_encapsulated_ac_pdu(AC, AC_PDU),
			{next_state, wait_for_user_requests, NewStateData#state{secure_transport_required = true}};
		% false
		false ->
			% store AC and user data
			NewStateData = StateData#state{application_context = AC, user_data = AC_PDU},
			{next_state, wait_for_user_requests, StateData#state{secure_transport_required = false}}
	end;
% @reference 3GPP TS 29.002 Figure 15.6/3j: Process_Secure_MAP_DSM (sheet 10)
idle({'TC', 'BEGIN', indication, #'TR-BEGIN'{userInfo = undefined, componentsPresent = true}}, StateData) ->
	% AC included? (false)
	% components present? (true)
	{next_state, wait_for_init_data, StateData};
idle({'TC', 'BEGIN', indication, Begin#'TR-BEGIN'{userInfo = AC}}, StateData) ->
	% AC included? (true)
	% AC version = 1? (true)
	% TODO:  the rest of it!
	{next_state, idle, StateData};
idle({'TC', 'BEGIN', indication, Parms} = Event, StateData) ->
	{next_state, wait_for_load_check_result1, StateData};
% forward unrecognized events to user callback module 
idle(Event, StateData) ->
	Module = StateData#state.module,
	ExtStateName = State#state.ext_statename,
	case Module:StateName(Event, StateData#state.ext_statedata) of
		{next_state, NextExtStateName, NewExtStateData} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{next_state, idle, NewStateData};
		{next_state, NextExtStateName, NewExtStateData, Timeout} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{next_state, idle, NewStateData, Timeout};
		{stop, Reason, NewExtStateData} ->
			NewStateData = StateData#state{ext_statedata = NewExtStateData},
			{stop, Reason, NewStateData};
		Other ->
			Other
	end.

%% @spec(Event, StateData) -> Result
%%
%% @doc State handler for the wait_for_user_requests state.
%%
%% @hidden
%%
% @reference 3GPP TS 29.002 15.2.1 Behaviour at the initiating side
% @reference 3GPP TS 29.002 Figure 15.6/3b: Process_Secure_MAP_DSM (sheet 2)
wait_for_user_requests({'MAP', 'DELIMITER', request, Parms} = Event, StateData) ->
	% TC_BEGIN_req_VIA_TC1
	{next_state, dialogue_initiated, StateData};
wait_for_user_requests({'MAP', 'U-ABORT', request, Parms} = Event, StateData) ->
	% Set_Abort_Reason: User_Specific
	% Set_User_Info: MAP_User_Abort_PDU
	% TC_U_ABORT_req_VIA_TC1
	% secure transport required?
	% true
		% Note: To all active SRSSMs
		% Terminated_VIA_Intern4
	% false
		% Note: To all active RSSMs
		% Terminated_VIA_Intern2
	{next_state, idle, StateData};
% any MAP specific request primitive
%wait_for_user_requests({'MAP', MapSpecificRequest, request, Parms} = Event, StateData) ->
	% secure transport required?
%	case StateData#state.secure_transport_required of
	% true
		% store request
		% spawn Secure_Requesting_MAP_SSM
		% Service_Invoked_VIA_Intern4
	% false
		% spawn Requesting_MAP_SSM
		% Service_Invoked_VIA_Intern2
%	{next_state, wait_for_user_requests, StateData};
% forward unrecognized events to user callback module 
wait_for_user_requests(Event, StateData) ->
	Module = StateData#state.module,
	ExtStateName = State#state.ext_statename,
	case Module:StateName(Event, StateData#state.ext_statedata) of
		{next_state, NextExtStateName, NewExtStateData} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{next_state, wait_for_user_requests, NewStateData};
		{next_state, NextExtStateName, NewExtStateData, Timeout} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{next_state, wait_for_user_requests, NewStateData, Timeout};
		{stop, Reason, NewExtStateData} ->
			NewStateData = StateData#state{ext_statedata = NewExtStateData},
			{stop, Reason, NewStateData};
		Other ->
			Other
	end.

%% @spec(Event, StateData) -> Result
%%
%% @doc State handler for the dialogue_initiated state.
%%
%% @hidden
%%
% @reference 3GPP TS 29.002 Figure 15.6/3c: Process_Secure_MAP_DSM (sheet 3)
dialogue_initiated({'TC', 'END', indication, End#'TC-END'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, idle, StateData};
dialogue_initiated({'TC', 'NOTICE', indication, Notice#'TC-NOTICE'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, idle, StateData};
% @reference 3GPP TS 29.002 Figure 15.6/3d: Process_Secure_MAP_DSM (sheet 4)
dialogue_initiated({'TC', 'P-ABORT', indication, Abort#'TC-P-ABORT'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, idle, StateData};
% @reference 3GPP TS 29.002 Figure 15.6/3e: Process_Secure_MAP_DSM (sheet 5)
dialogue_initiated({'TC', 'U-ABORT', indication, Abort#'TC-U-ABORT'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, idle, StateData};
% @reference 3GPP TS 29.002 Figure 15.6/3e: Process_Secure_MAP_DSM (sheet 5)
% @reference 3GPP TS 29.002 Figure 15.6/3f: Process_Secure_MAP_DSM (sheet 6)
% @reference 3GPP TS 29.002 Figure 15.6/3g: Process_Secure_MAP_DSM (sheet 7)
dialogue_initiated({'TC', 'U-ABORT', indication, Abort#'TC-U-ABORT'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, dialogue_initiated, StateData};
dialogue_initiated({'TC', 'L-CANCEL', indication, Cancel#'TC-L-CANCEL'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, dialogue_initiated, StateData};
% @reference 3GPP TS 29.002 Figure 15.6/3h: Process_Secure_MAP_DSM (sheet 8)
dialogue_initiated({'MAP', 'U-ABORT', request, Abort#'MAP-U-ABORT'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, idle, StateData};
dialogue_initiated({'MAP', 'CLOSE', request, Close#'MAP-CLOSE'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, idle, StateData};
% @reference 3GPP TS 29.002 Figure 15.6/3i: Process_Secure_MAP_DSM (sheet 9)
dialogue_initiated({'TC', 'CONTINUE', indication, Continue#'TC-Continue'{}} = Event, StateData) ->
	% Note: the first one
	% TODO: the rest!
	{next_state, dialogue_established, StateData};
% forward unrecognized events to user callback module 
dialogue_initiated(Event, StateData) ->
	Module = StateData#state.module,
	ExtStateName = State#state.ext_statename,
	case Module:StateName(Event, StateData#state.ext_statedata) of
		{next_state, NextExtStateName, NewExtStateData} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{next_state, dialogue_initiated, NewStateData};
		{next_state, NextExtStateName, NewExtStateData, Timeout} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{next_state, dialogue_initiated, NewStateData, Timeout};
		{stop, Reason, NewExtStateData} ->
			NewStateData = StateData#state{ext_statedata = NewExtStateData},
			{stop, Reason, NewStateData};
		Other ->
			Other
	end.

%% @spec(Event, StateData) -> Result
%%
%% @doc State handler for the wait_for_init_data state.
%%
%% @hidden
%%
% @reference 3GPP TS 29.002 Figure 15.6/3k: Process_Secure_MAP_DSM (sheet 11)
wait_for_init_data({'TC', 'INVOKE', indication, Invoke#'TC-INVOKE'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, wait_for_load_check_result2, StateData};
wait_for_init_data({'TC', 'L-REJECT', indication, Reject#'TC-L-REJECT'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, idle, StateData};
% any other indication
wait_for_init_data({'TC', _, indication, _} = Event, StateData) ->
	% TC_U_ABORT_req_VIA_TC1
	{next_state, idle, StateData};
% forward unrecognized events to user callback
wait_for_init_data(Event, StateData) ->
	Module = StateData#state.module,
	ExtStateName = State#state.ext_statename,
	case Module:StateName(Event, StateData#state.ext_statedata) of
		{next_state, NextExtStateName, NewExtStateData} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{next_state, wait_for_init_data, NewStateData};
		{next_state, NextExtStateName, NewExtStateData, Timeout} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{next_state, wait_for_init_data, NewStateData, Timeout};
		{stop, Reason, NewExtStateData} ->
			NewStateData = StateData#state{ext_statedata = NewExtStateData},
			{stop, Reason, NewStateData};
		Other ->
			Other
	end.

%% @spec(Event, StateData) -> Result
%%
%% @doc State handler for the wait_for_load_check_result2 state.
%%
%% @hidden
%%
% @reference 3GPP TS 29.002 Figure 15.6/3k: Process_Secure_MAP_DSM (sheet 11)
wait_for_load_check_result2('Load_OK', StateData) ->
	% TODO: the rest!
	{next_state, idle, StateData};
wait_for_load_check_result2('Overload', StateData) ->
	{next_state, idle, StateData};
% any other indication
wait_for_load_check_result2({'TC', _, indication, _} = Event, StateData) ->
	% TC_U_ABORT_req_VIA_TC1
	{next_state, idle, StateData};
% forward unrecognized events to user callback module
wait_for_load_check_result2(Event, StateData) ->
	Module = StateData#state.module,
	ExtStateName = State#state.ext_statename,
	case Module:StateName(Event, StateData#state.ext_statedata) of
		{next_state, NextExtStateName, NewExtStateData} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{next_state, wait_for_load_check_result2, NewStateData};
		{next_state, NextExtStateName, NewExtStateData, Timeout} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{next_state, wait_for_load_check_result2, NewStateData, Timeout};
		{stop, Reason, NewExtStateData} ->
			NewStateData = StateData#state{ext_statedata = NewExtStateData},
			{stop, Reason, NewStateData};
		Other ->
			Other
	end.

%% @spec(Event, StateData) -> Result
%%
%% @doc State handler for the wait_for_load_check_result1 state.
%%
%% @hidden
%%
% @reference 3GPP TS 29.002 Figure 15.6/3l: Process_Secure_MAP_DSM (sheet 12)
wait_for_load_check_result1('Load_OK', StateData) ->
	% TODO: the rest!
	{next_state, idle, StateData};
wait_for_load_check_result1('Load_OK', StateData) ->
	% TODO: the rest!
	{next_state, dialogue_pending, StateData};
wait_for_load_check_result1('Overload', StateData) ->
	{next_state, idle, StateData};
% any other indication
wait_for_load_check_result1({'TC', _, indication, _} = Event, StateData) ->
	% TC_U_ABORT_req_VIA_TC1
	{next_state, idle, StateData};
% forward unrecognized events to user callback module
wait_for_load_check_result1(Event, StateData) ->
	Module = StateData#state.module,
	ExtStateName = State#state.ext_statename,
	case Module:StateName(Event, StateData#state.ext_statedata) of
		{next_state, NextExtStateName, NewExtStateData} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{next_state, wait_for_load_check_result1, NewStateData};
		{next_state, NextExtStateName, NewExtStateData, Timeout} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{next_state, wait_for_load_check_result1, NewStateData, Timeout};
		{stop, Reason, NewExtStateData} ->
			NewStateData = StateData#state{ext_statedata = NewExtStateData},
			{stop, Reason, NewStateData};
		Other ->
			Other
	end.

%% @spec(Event, StateData) -> Result
%%
%% @doc State handler for the dialogue_pending state.
%%
%% @hidden
%%
% @reference 3GPP TS 29.002 Figure 15.6/3m: Process_Secure_MAP_DSM (sheet 13)
dialogue_pending({'MAP', 'OPEN', response, Open#'MAP-OPEN'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, dialogue_accepted, StateData};
dialogue_pending({'MAP', 'OPEN', response, Open#'MAP-OPEN'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, idle, StateData};
dialogue_pending({'MAP', 'U-ABORT', request, Abort#'MAP-U-ABORT'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, idle, StateData};
% forward unrecognized events to user callback module 
dialogue_pending(Event, StateData) ->
	Module = StateData#state.module,
	ExtStateName = State#state.ext_statename,
	case Module:StateName(Event, StateData#state.ext_statedata) of
		{next_state, NextExtStateName, NewExtStateData} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{next_state, dialogue_pending, NewStateData};
		{next_state, NextExtStateName, NewExtStateData, Timeout} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{next_state, dialogue_pending, NewStateData, Timeout};
		{stop, Reason, NewExtStateData} ->
			NewStateData = StateData#state{ext_statedata = NewExtStateData},
			{stop, Reason, NewStateData};
		Other ->
			Other
	end.

%% @spec(Event, StateData) -> Result
%%
%% @doc State handler for the dialogue_accepted state.
%%
%% @hidden
%%
% @reference 3GPP TS 29.002 Figure 15.6/3n: Process_Secure_MAP_DSM (sheet 14)
dialogue_accepted({'MAP', 'CLOSE', request, Close#'MAP-CLOSE'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, idle, StateData};
dialogue_accepted({'MAP', 'DELIMITER', request, Delimiter#'MAP-DELIMITER'{}} = Event, StateData) ->
	% TC_CONTINUE_req_VIA_TC1
	{next_state, dialogue_established, StateData};
% any MAP specific request primitive
dialogue_accepted({'MAP', Request, request, Parm} = Event, StateData) ->
	% TODO: the rest!
	{next_state, dialogue_accepted, StateData};
% any MAP specific response primitive
dialogue_accepted({'MAP', Response, response, Parm} = Event, StateData) ->
	% TODO: the rest!
	{next_state, dialogue_accepted, StateData};
% forward unrecognized events to user callback module 
dialogue_accepted(Event, StateData) ->
	Module = StateData#state.module,
	ExtStateName = State#state.ext_statename,
	case Module:StateName(Event, StateData#state.ext_statedata) of
		{next_state, NextExtStateName, NewExtStateData} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{next_state, dialogue_accepted, NewStateData};
		{next_state, NextExtStateName, NewExtStateData, Timeout} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{next_state, dialogue_accepted, NewStateData, Timeout};
		{stop, Reason, NewExtStateData} ->
			NewStateData = StateData#state{ext_statedata = NewExtStateData},
			{stop, Reason, NewStateData};
		Other ->
			Other
	end.

%% @spec(Event, StateData) -> Result
%%
%% @doc State handler for the dialogue_established state.
%%
%% @hidden
%%
% @reference 3GPP TS 29.002 Figure 15.6/3o: Process_Secure_MAP_DSM (sheet 15)
dialogue_established({'TC', 'L-CANCEL', indication, Cancel#'TC-L-CANCEL'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, dialogue_established, StateData};
dialogue_established({'TC', 'NOTICE', indication, Notice#'TC-NOTICE'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, dialogue_established, StateData};
dialogue_established({'TC', 'CONTINUE', indication, Continue#'TC-CONTINUE'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, dialogue_established, StateData};
dialogue_established({'MAP', 'DELIMITER', request, Delimiter#'MAP-DELIMITER'{}} = Event, StateData) ->
	% TC_CONTINUE_req_VIA_TC1
	{next_state, dialogue_established, StateData};
% @reference 3GPP TS 29.002 Figure 15.6/3p: Process_Secure_MAP_DSM (sheet 16)
dialogue_established({'TC', 'END', indication, End#'TC-END'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, idle, StateData};
dialogue_established({'MAP', 'CLOSE', request, Close#'MAP-CLOSE'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, dialogue_established, StateData};
% @reference 3GPP TS 29.002 Figure 15.6/3q: Process_Secure_MAP_DSM (sheet 17)
dialogue_established({'TC', 'U-ABORT', indication, Abort#'TC-U-ABORT'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, idle, StateData};
dialogue_established({'TC', 'P-ABORT', indication, Abort#'TC-P-ABORT'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, idle, StateData};
dialogue_established({'TC', 'U-ABORT', request, Abort#'TC-U-ABORT'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, idle, StateData};
% @reference 3GPP TS 29.002 Figure 15.6/3o: Process_Secure_MAP_DSM (sheet 15)
% any MAP specific request primitive
dialogue_established({'MAP', Request, request, Parm} = Event, StateData) ->
	% TODO: the rest!
	{next_state, dialogue_established, StateData};
% any MAP specific response primitive
dialogue_established({'MAP', Response, response, Parm} = Event, StateData) ->
	% TODO: the rest!
	{next_state, dialogue_established, StateData};
% forward unrecognized events to user callback module 
dialogue_established(Event, StateData) ->
	Module = StateData#state.module,
	ExtStateName = State#state.ext_statename,
	case Module:StateName(Event, StateData#state.ext_statedata) of
		{next_state, NextExtStateName, NewExtStateData} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{next_state, dialogue_established, NewStateData};
		{next_state, NextExtStateName, NewExtStateData, Timeout} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{next_state, dialogue_established, NewStateData, Timeout};
		{stop, Reason, NewExtStateData} ->
			NewStateData = StateData#state{ext_statedata = NewExtStateData},
			{stop, Reason, NewStateData};
		Other ->
			Other
	end.

%% @spec(Event, StateData) -> Result
%%
%% @doc State handler for the wait_for_components state.
%%
%% @hidden
%%
% @reference 3GPP TS 29.002 Figure 15.6/4a: Procedure Process_Components (sheet 1)
% @reference 3GPP TS 29.002 Figure 15.6/4b: Procedure Process_Components (sheet 2)
wait_for_components({'TC', 'INVOKE', indication, Invoke#'TC-INVOKE'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, wait_for_components, StateData};
% @reference 3GPP TS 29.002 Figure 15.6/4c: Procedure Process_Components (sheet 3)
wait_for_components({'TC', 'RESULT-L', indication, Result#'TC-RESULT-L'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, wait_for_components, StateData};
wait_for_components({'TC', 'RESULT-NL', indication, Result#'TC-RESULT-NL'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, wait_for_components, StateData};
wait_for_components({'TC', 'U-ERROR', indication, Error#'TC-U-ERROR'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, wait_for_components, StateData};
% @reference 3GPP TS 29.002 Figure 15.6/4d: Procedure Process_Components (sheet 4)
wait_for_components({'TC', 'L-REJECT', indication, Reject#'TC-L-REJECT'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, wait_for_components, StateData};
wait_for_components({'TC', 'R-REJECT', indication, Reject#'TC-R-REJECT'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, wait_for_components, StateData};
% @reference 3GPP TS 29.002 Figure 15.6/4e: Procedure Process_Components (sheet 5)
wait_for_components({'TC', 'U-REJECT', indication, Reject#'TC-U-REJECT'{}} = Event, StateData) ->
	% TODO: the rest!
	{next_state, wait_for_components, StateData};
% forward unrecognized events to user callback module 
wait_for_components(Event, StateData) ->
	Module = StateData#state.module,
	ExtStateName = State#state.ext_statename,
	case Module:StateName(Event, StateData#state.ext_statedata) of
		{next_state, NextExtStateName, NewExtStateData} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{next_state, wait_for_components, NewStateData};
		{next_state, NextExtStateName, NewExtStateData, Timeout} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{next_state, wait_for_components, NewStateData, Timeout};
		{stop, Reason, NewExtStateData} ->
			NewStateData = StateData#state{ext_statedata = NewExtStateData},
			{stop, Reason, NewStateData};
		Other ->
			Other
	end.
%% @hidden
% accept TC service primitives sent with ! or erlang:send/2,3
	handle_info({'TC', _Primitive, indication, _Parms} = Info, StateName, StateData) ->
		?MODULE:StateName(Info, StateData);
% forward unrecognized info to user callback module 
handle_info(Event, Statename, StateData) ->
	Module = StateData#state.module,
	ExtStateName = StateData#state.ext_statename,
	ExtStateData = StateData#state.ext_statedata,
	case Module:handle_info(Event, ExtStateName, StateData#state.ext_statedata) of
		{next_state, NextExtStateName, NewExtStateData} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{next_state, StateName, NewStateData};
		{next_state, NextExtStateName, NewExtStateData, Timeout} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{next_state, StateName, NewStateData, Timeout};
		{stop, Reason, NewExtStateData} ->
			NewStateData = StateData#state{ext_statedata = NewExtStateData},
			{stop, Reason, NewStateData};
		Other ->
			Other
	end.

%% @hidden
terminate(Reason, StateName, StateData) ->
	Module = StateData#state.module,
	ExtStateName = StateData#state.ext_statename,
	ExtStateData = StateData#state.ext_statedata,
	Module:terminate(Reason, ExtStateName, ExtStateData).

%% @hidden
code_change(OldVersion, StateName, StateData, Extra) ->
	Module = StateData#state.module,
	ExtStateName = StateData#state.ext_statename,
	ExtStateData = StateData#state.ext_statedata,
	case Module:code_change(OldVersion, ExtStateName, ExtStateData, Extra) of
		{ok, NextExtStateName, NewExtStateData} ->
			NewStateData = StateData#state{ext_statename = NextExtStateName, ext_statedata = NewExtStateData},
			{ok, StateName, NewStateData};
		Other ->
			Other
	end.


%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @spec(Parms, StateData) -> boolean()
%%
%% @doc Check whether secure transport is required.
%%
%% @reference 3GPP TS 29.002 15.2.1 Behaviour at the initiating side
%% @reference 3GPP TS 33.200
%%
%% @hidden
%%
secure_transport_required(Parms, #state{application_context = 'secureTransportHandlingContext-v3'}) ->
	true;
secure_transport_required(Parms, #state{}) ->
	false;

%% @spec(StateData) -> NewStateData
%%
%% @doc Build an encapsulted AC PDU.
%%
%% @reference 3GPP TS 29.002 15.2.1 Behaviour at the initiating side
%% @reference 3GPP TS 33.200
%%
%% @hidden
%%
build_encapsulated_ac_pdu(StateData) ->
	% TODO:  Build an encapsulted AC PDU.
	StateData.

