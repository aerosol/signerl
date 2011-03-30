%%% $Id: tcap_ism_fsm.erl,v 1.3 2005/08/04 09:33:17 vances Exp $
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
%%% @doc Invocation State Machine (ISM) functional block within the
%%% 		component sub-layer of ITU TCAP.
%%%
%%% @reference ANSI T1.114.4 Transaction Capabilities Procedures 
%%% @reference ITU-T Q.774 (06/97) Annex A Transaction capabilities SDLs
%%%
%%% @private
%%%

-module(tcap_ism_fsm).
-copyright('Copyright (c) 2004-2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision: 1.3 $').

-behaviour(gen_fsm).

%% call backs needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_info/3, terminate/3, code_change/4]).

%% invocation_fsm state callbacks 
-export([]).

%% record definitions for TC-User primitives
-include("tcap.hrl").
%% record definitions for TCAP messages
%-include("TCAPMessages.hrl").

%% the invocation_fsm state data
-record(state, {usap, dialogueID, cco}).

%%----------------------------------------------------------------------
%%  The gen_fsm call backs
%%----------------------------------------------------------------------

%% Start the Invocation State Machine (ISM) process
%% reference: Figure A.7/Q.774 (sheet 1 of 6)


%% handle any other message
handle_info(Info, StateName, State) ->
	error_logger:format("~w (~w) received unexpected message: ~w~n", [?Module, self(), Event]),
	{next_state, StateName, State};

%% handle a shutdown request
terminate(_Reason, _StateName, State) -> ok.

%% handle updating state data due to a code replacement
code_change(OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

