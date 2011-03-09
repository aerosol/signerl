%%% $Id: map_sup.erl,v 1.1 2005/02/13 00:42:12 vances Exp $
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
%%% @doc MAP application top level supervisor.
%%%
%%% @reference <a href="index.html">MAP User's Guide</a>
%%%
%%% @private
         
         
-module(map_sup).
-copyright('Copyright (c) 2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision: 1.1 $').

-behaviour(supervisor).

%% call backs needed for supervisor behaviour
-export([init/1]).

%% @spec(StartArgs::term()) -> Result = {ok,{{RestartStrategy,MaxR,MaxT},[ChildSpec]}} | ignore
%% 	RestartStrategy = one_for_all | one_for_one | rest_for_one | simple_one_for_one
%% 	MaxR = MaxT = int()>=0
%% 	ChildSpec = child_spec()
%%
%% @equiv //stdlib/supervisor:init/1
%%
init(_StartArgs) ->
	StartMod = map_pm_sup,
	StartFunc = {supervisor, start_link, [StartMod]},
	ChildSpec = {pm_sup, StartFunc, permanent, infinity, supervisor, [StartMod]},
	{ok,{{simple_one_for_one, 10, 60}, ChildSpec}}.

