%%% $Id: tcap_cco_server.erl,v 1.3 2005/08/04 09:33:17 vances Exp $
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
%%% @doc TCAP Component Coordinator (CCO) functional block within the
%%% 		component sub-layer of ANSI TCAP.
%%%
%%% @reference ANSI T1.114.4 Transaction Capabilities Procedures 
%%% @reference ITU-T Q.774 (06/97) Annex A Transaction capabilities SDLs
%%%
%%% @private
%%%
         
-module(tcap_cco_server).
-copyright('Copyright (c) 2004-2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision: 1.3 $').

-behaviour(gen_server).

%% call backs needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {supervisor, usap, dialogueID}).

%%----------------------------------------------------------------------
%%  The gen_server call backs
%%----------------------------------------------------------------------

%% initialize the server
init([Supervisor, USAP, DialogueID]) ->
	process_flag(trap_exit, true),
	{ok, #state{supervisor = Supervisor, usap = USAP, dialogueID = DialogueID}}.

%% shutdown the server
handle_call(stop, _From, State) ->
	{stop, shutdown, ok, State};

%% unrecognized calls
handle_call(Other, From, State) ->
	error_logger:error_report([{unknown_call, Other}, {from, From}]),
	{noreply, State}.

%% unrecognized casts
handle_cast(Other, State) ->
	error_logger:error_report([{unknown_cast, Other}]),
	{noreply, State}.


%% trapped exit signals
handle_info({'EXIT', _Pid, Reason}, State) ->
	{stop, Reason, State};

%% unknown messages
handle_info(Unknown, State) ->
	error_logger:error_msg("Received unknown message: ~p~n", [Unknown]),
	{noreply, State}.

%% someone wants us to shutdown and cleanup
terminate(_Reason, _State) -> ok.

%% upgrading the running code
code_change(_, _, _) -> ok.

%%%
%%% internal functions
%%%

