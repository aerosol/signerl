%%% $Id: tcap_ism_fsm.erl,v 1.3 2005/08/04 09:33:17 vances Exp $
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
%-export([init/1, handle_event/3, handle_info/3, terminate/3, code_change/4]).
-export([handle_info/3, terminate/3, code_change/4]).

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
	error_logger:format("~w (~w) received unexpected message: ~w~n", [?MODULE, self(), Info]),
	{next_state, StateName, State}.

%% handle a shutdown request
terminate(_Reason, _StateName, State) -> ok.

%% handle updating state data due to a code replacement
code_change(OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

