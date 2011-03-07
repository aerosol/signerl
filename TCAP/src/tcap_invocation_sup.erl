%%% $Id: tcap_invocation_sup.erl,v 1.2 2005/08/04 09:33:17 vances Exp $
%%%---------------------------------------------------------------------
%%% @copyright 2004-2005 Motivity Telecom Inc.
%%% @author Vance Shipley <vances@motivity.ca>  [http://www.motivity.ca]
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
         
-module(tcap_invocation_sup).
-copyright('Copyright (c) 2003-2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision: 1.2 $').

-behaviour(supervisor).

%% call backs needed for supervisor behaviour
-export([init/1]).

init([USAP, InvokeID]) ->
	Name = list_to_atom("ism_" ++ integer_to_list(InvokeID)),
	StartArgs = [tcap_ism_fsm, [USAP, InvokeID], []],
	StartFunc = {gen_fsm, start_link, StartArgs},
	ChildSpec = {Name, StartFunc, temporary, 4000, worker,
			[tcap_ism_sup]},
	{ok,{{one_for_all, 0, 1}, [ChildSpec]}}.

