%%%
%%%---------------------------------------------------------------------
%%% @copyright 2011 Harald Welte
%%% @author Harald Welte <laforge@gnumonks.org>
%%% @end
%%%
%%% Copyright (c) 2011 Harald Welte
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
%%% @doc TCAP application user SAP helper functions.
%%%
%%% @reference <a href="index.html">TCAP User's Guide</a>
%%%
%%% @private

-module(tcap_user).
-copyright('Copyright (c) 2011 Harald Welte').
-author('Harald Welte <laforge@gnumonks.org>').

-export([send_prim/2, start_sap/3]).

-include("tcap.hrl").

% the user (TCU) sends us a primitive.  We decide where to route it
send_prim(TCO, P={'TC', 'INVOKE', request, Param}) when
				is_record(Param, 'TC-INVOKE') ->
	DialgId = Param#'TC-INVOKE'.dialogueID,
	case ets:lookup(tcap_dha, DialgId) of
	    [] ->
		% start new DHA FSM for this dialogue; it will start CCO
		DHA = start_new_dha(TCO, DialgId);
	    [DHA] ->
		ok
	end,
	% resolve CCO from DHA
	CCO = tcap_dha_fsm:get_cco_pid(DHA),
	gen_server:cast(CCO, P);
send_prim(TCO, P={'TC', 'BEGIN', request, Param}) when
				is_record(Param, 'TC-BEGIN') ->
	DialgId = Param#'TC-BEGIN'.dialogueID,
	case ets:lookup(tcap_dha, DialgId) of
	    [] ->
		% start new DHA FSM for this dialogue; it will start CCO
		DHA = start_new_dha(TCO, DialgId);
	    [DHA] ->
		    ok
	end,
	gen_fsm:send_event(DHA, P);
send_prim(_TCO, P) ->
	{errror, {unknown_prim, P}}.

% high-level user API to start the TCAP SAP supervisor + TCO server for a given SAP
start_sap(SccpModule, Args, Opts) ->
	% tcap_sup os simple_one_for_one and thus will start a tcap_sap_sup
	% process that will in turn start the TCO "Module" as a child.  The pid
	% we get is the tcap_sap_sup, not the TCO itself!
	case supervisor:start_child(tcap_sup, [SccpModule, Args, Opts]) of
		{ok, TcoSupPid} ->
			[{_, TCO, _, _}] = supervisor:which_children(TcoSupPid),
			{ok, TCO};
		Default ->
			Default
	end.

%% local functions

start_new_dha(TCO, LocalTID) ->
	Args = [self(), LocalTID, TCO],
	{ok, Pid} = supervisor:start_link(tcap_dialogue_sup, Args),
	Pid.

