%%% $Id: map_app.erl,v 1.1 2005/02/13 00:42:12 vances Exp $
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
%%% @doc MAP application callback module.
%%%
%%% @reference <a href="index.html">MAP User's Guide</a>
%%%
%%% @private
         
-module(map_app).
-copyright('Copyright (c) 2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision: 1.1 $').

%% export application behaviour callbacks
-export([start/2, start_phase/3, prep_stop/1, stop/1, config_change/3]).


%% @spec(StartType, StartArgs::term()) -> {ok, Pid} | {ok, Pid, State} | {error, Reason}
%% 	StartType = normal | {takeover,Node} | {failover,Node}
%% 	Node = node()
%% 	Pid = pid()
%% 	State = term()
%% 	Reason = term()
%%
%% @equiv //kernel/application:start/3
%%
start(normal, StartArgs) ->
	{ok, SupRef} = application:get_env(supref),
	supervisor:start_link(SupRef, map_sup, StartArgs).

%% @spec(Phase::atom(), StartType, PhaseArgs::term()) -> ok | {error, Reason}
%% 	StartType = normal | {takeover,Node} | {failover,Node}
%% 	Node = node()
%% 	Reason = term()
%%
%% @equiv //kernel/application:start_phase/3
%%
start_phase(_Phase, _StartType, _PhaseArgs) ->
	ok.

%% @spec(State::term()) -> NewState
%% 	NewState = term()
%%
%% @equiv //kernel/application:prep_stop/1
%%
prep_stop(State) ->
	State.

%% @spec(State) -> ok
%%
%% @equiv //kernel/application:stop/1
%%
stop(_State) ->
	ok.

%% @spec(Changed, New, Removed) ->
%% 	Changed = [{Par,Val}]
%% 	New = [{Par,Val}]
%% 	Removed = [Par]
%% 	Par = atom()
%% 	Val = term()
%%
%% @equiv //kernel/application:config_change/3
%%
config_change(_Changed, _New, _Removed) ->
	ok.

