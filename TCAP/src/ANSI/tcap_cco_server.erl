%%% $Id: tcap_cco_server.erl,v 1.3 2005/08/04 09:33:17 vances Exp $
%%%---------------------------------------------------------------------
%%% @copyright 2004-2005 Motivity Telecom Inc.
%%% @author Vance Shipley <vances@motivity.ca> [http://www.motivity.ca]
%%% @end
%%%
%%% Copyright Motivity Telecom Inc. 2004-2005
%%%
%%% All rights reserved. No part of this computer program(s) may be
%%% used, reproduced, stored in any retrieval system, or transmitted,
%%% in any form or by any means, electronic, mechanical, photocopying,
%%% recording, or otherwise without prior written permission of
%%% Motivity Telecom Inc.
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

