%%% $Id: tcap_app.erl,v 1.3 2005/08/04 09:33:17 vances Exp $
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
%%% @doc TCAP application callback module.
%%%
%%% @reference <a href="index.html">TCAP User's Guide</a>
%%%
%%% @private
         
-module(tcap_app).
-copyright('Copyright (c) 2004-2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision: 1.3 $').

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
	ets:new(tcap_transaction, [named_table, public]),
	ets:new(tcap_dha, [named_table, public]),
	{ok, SupRef} = application:get_env(supref),
	supervisor:start_link(SupRef, tcap_sup, StartArgs).

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

