%%% $Id: tcap_transaction_sup.erl,v 1.2 2005/08/04 09:33:17 vances Exp $
%%%---------------------------------------------------------------------
%%% @copyright 2004-2005 Motivity Telecom, 2010-2011 Harald Welte
%%% @author Vance Shipley <vances@motivity.ca>, Harald Welte <laforge@gnumonks.org>
%%% @end
%%%
%%% Copyright (c) 2004-2005, Motivity Telecom
%%% Copyright (c) 2010-2011, Harald Welte
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
         
-module(tcap_transaction_sup).
-copyright('Copyright (c) 2003-2005 Motivity Telecom Inc., 2010-2011 Harald Welte').
-author('vances@motivity.ca, laforge@gnumonks.org').
-vsn('$Revision: 1.2 $').

-behaviour(supervisor).

%% call backs needed for supervisor behaviour
-export([init/1]).

gen_tsm_childspec(NSAP, USAP, TID, TCO) ->
	Name = list_to_atom("tcap_tsm_" ++ integer_to_list(TID)),
	StartArgs = [{local, Name}, tcap_tsm_fsm, [NSAP, USAP, TID, self(), TCO], [{debug, [trace]}]],
	StartFunc = {gen_fsm, start_link, StartArgs},
	{Name, StartFunc, permanent, 1000, worker, [tcap_tsm_fsm]}.

gen_dha_sup_childspec(_NSAP, User, LocalTID, TCO) ->
	Name = list_to_atom("tcap_dialogue_sup_" ++ integer_to_list(LocalTID)),
	StartArgs = [{local, Name}, tcap_dialogue_sup, {User, LocalTID, TCO}],
	StartFunc = {supervisor, start_link, StartArgs},
	{dha_sup, StartFunc, permanent, 1000, worker, [tcap_dialogue_sup]}.

init([NSAPfun, USAP, TID, TCO]) ->
	TsmChild = gen_tsm_childspec(NSAPfun, USAP, TID, TCO),
	DhaSupChild = gen_dha_sup_childspec(NSAPfun, USAP, TID, TCO),
	{ok,{{one_for_all, 0, 1}, [TsmChild, DhaSupChild]}}.
