%%% $Id: tcap_sap_sup.erl,v 1.5 2005/08/04 09:33:17 vances Exp $
%%%---------------------------------------------------------------------
%%% @copyright 2004-2005 Motivity Telecom Inc.
%%% @author Vance Shipley <vances@motivity.ca> [http://www.motivity.ca]
%%% @end
%%%
%%% Copyright Motivity Telecom Inc. 2003-2005
%%%
%%% All rights reserved. No part of this computer program(s) may be
%%% used, reproduced, stored in any retrieval system, or transmitted,
%%% in any form or by any means, electronic, mechanical, photocopying,
%%% recording, or otherwise without prior written permission of
%%% Motivity Telecom Inc.
%%%---------------------------------------------------------------------
%%%
%%% @doc TCAP service access point supervisor.
%%%
%%% @reference <a href="index.html">TCAP User's Guide</a>
%%%
%%% @private
         
-module(tcap_sap_sup).
-copyright('Copyright (c) 2003-2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision: 1.5 $').

-behaviour(supervisor).

-export([init/1]).


%% @spec(StartArgs::term()) -> Result = {ok,{{RestartStrategy,MaxR,MaxT},[ChildSpec]}} | ignore
%% 	RestartStrategy = one_for_all | one_for_one | rest_for_one | simple_one_for_one
%% 	MaxR = MaxT = int()>=0
%% 	ChildSpec = child_spec()
%%
%% @see supervisor:init/1
%%
init([Module, Args, Options]) when is_list(Args), is_list(Options) ->
	% Name = list_to_atom("tcap_ssn" ++ integer_to_list(SSN)),
	StartArgs = [Module, [self()] ++ Args, Options],
	StartFunc = {gen_server, start_link, StartArgs},
	ChildSpec = {tco, StartFunc, permanent, 4000, worker, [Module, tcap_tco_server]},
	{ok,{{one_for_one, 10, 60}, [ChildSpec]}}.

