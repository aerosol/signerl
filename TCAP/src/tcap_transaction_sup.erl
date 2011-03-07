%%% $Id: tcap_transaction_sup.erl,v 1.2 2005/08/04 09:33:17 vances Exp $
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
         
-module(tcap_transaction_sup).
-copyright('Copyright (c) 2003-2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision: 1.2 $').

-behaviour(supervisor).

%% call backs needed for supervisor behaviour
-export([init/1]).

init({NSAP, USAP, TID, SupRef}) ->
	Name = list_to_atom("tsm_" ++ integer_to_list(TID)),
	StartArgs = [tcap_tsm_fsm, [NSAP, USAP, TID, self(), SupRef], []],
	StartFunc = {gen_fsm, start_link, StartArgs},
	ChildSpec = {Name, StartFunc, permanent, 1000, worker, [tcap_tsm_fsm]},
	{ok,{{one_for_all, 0, 1}, [ChildSpec]}}.

