%%% $Id: map_pm_sup.erl,v 1.1 2005/02/13 00:42:12 vances Exp $
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
%%% @doc MAP service protocol machine supervisor.
%%%
%%% @reference <a href="index.html">MAP User's Guide</a>
%%%
%%% @private
         
-module(tcap_pm_sup).
-copyright('Copyright (c) 2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision: 1.1 $').

-behaviour(supervisor).

-export([init/1]).


%% @spec(StartArgs::term()) -> Result = {ok,{{RestartStrategy,MaxR,MaxT},[ChildSpec]}} | ignore
%% 	RestartStrategy = one_for_all | one_for_one | rest_for_one | simple_one_for_one
%% 	MaxR = MaxT = int()>=0
%% 	ChildSpec = child_spec()
%%
%% @see supervisor:init/1
%%
init([Module, Args, Options]) when is_atom(Module), is_list(Args), is_list(Options) ->
	StartArgs = [Module, [self()] ++ Args, Options],
	StartFunc = {gen_fsm, start_link, StartArgs},
	ChildSpec = {dsm, StartFunc, permanent, 4000, worker, [Module, map_dsm_fsm]},
	{ok,{{one_for_one, 10, 60}, [ChildSpec]}}.

