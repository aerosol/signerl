
-module(tcap_user).

-export([send_prim/2]).

-include("tcap.hrl").

% the user (TCU) sends us a primitive.  We decide where to route it
send_prim(TCO, P={'TC', 'INVOKE', request, Param}) when
				is_record(Param, 'TC-INVOKE') ->
	DialgId = Param#'TC-INVOKE'.dialogueID,
	case ets:lookup(tcap_dha, DialgId) of
	    [] ->
		% start new DHA FSM for this dialogue; it will start CCO
		DHA = start_new_dha(TCO, DialgId);
	    [DHA] ->
		ok
	end,
	% resolve CCO from DHA
	CCO = tcap_dha_fsm:get_cco_pid(DHA),
	gen_server:cast(CCO, P);
send_prim(TCO, P={'TC', 'BEGIN', request, Param}) when
				is_record(Param, 'TC-BEGIN') ->
	DialgId = Param#'TC-BEGIN'.dialogueID,
	case ets:lookup(tcap_dha, DialgId) of
	    [] ->
		% start new DHA FSM for this dialogue; it will start CCO
		DHA = start_new_dha(TCO, DialgId);
	    [DHA] ->
		    ok
	end,
	gen_fsm:send_event(DHA, P);
send_prim(_TCO, P) ->
	{errror, {unknown_prim, P}}.


%% local functions

start_new_dha(TCO, LocalTID) ->
	Args = [self(), LocalTID, TCO],
	{ok, Pid} = supervisor:start_link(tcap_dialogue_sup, Args),
	Pid.

