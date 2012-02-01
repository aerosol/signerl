%%% $Id: tcap_ism_fsm.erl,v 1.3 2005/08/04 09:33:17 vances Exp $
%%%---------------------------------------------------------------------
%%% @copyright 2010-2011 Harald Welte
%%% @author Harald Welte <laforge@gnumonks.org>
%%% @end
%%%
%%% Copyright (c) 2010-2011, Harald Welte <laforge@gnumonks.org>
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
%%% @doc Invocation State Machine (ISM) functional block within the
%%% 		component sub-layer of ITU TCAP.
%%%
%%% @reference ANSI T1.114.4 Transaction Capabilities Procedures 
%%% @reference ITU-T Q.774 (06/97) Annex A Transaction capabilities SDLs
%%%
%%% @private
%%%

-module(tcap_ism_fsm).
-copyright('Copyright (c) 2010-2011 Harald Welte').
-author('laforge@gnumonks.org').
-vsn('$Revision: 1.3 $').

-behaviour(gen_fsm).

%% call backs needed for gen_fsm behaviour
-export([init/1, handle_event/3, handle_info/3, terminate/3, code_change/4]).

%% invocation_fsm state callbacks 
-export([start_link/5]).

-export([idle/2, op_sent_cl1/2, op_sent_cl2/2, op_sent_cl3/2,
	 op_sent_cl4/2, wait_for_reject/2]).

%% record definitions for TC-User primitives
-include("tcap.hrl").
%% record definitions for TCAP messages
%-include("TCAPMessages.hrl").

%% the invocation_fsm state data
-record(state, {
	usap,		% Pid of the TC-User
	dialogueId,
	invokeId,
	cco,		% Pid of the CCO
	op_class,	% operation class (1..4)
	inv_timeout,	% milliseconds
	inv_timer,	% timer()
	rej_timer	% timer()
}).

% value in milliseconds, spec doesn't say how long...
-define(REJECT_TIMER, 1 * 1000).

start_link(Usap, DialogueId, InvokeId, OpClass, InvTimeout) ->
	ProcName = list_to_atom("ism_fsm_" ++ integer_to_list(DialogueId)
				++ "_" ++ integer_to_list(InvokeId)),
	ArgL = [Usap, DialogueId, InvokeId, self(), OpClass, InvTimeout],
	gen_fsm:start_link({local, ProcName}, ?MODULE, ArgL, [{debug, [trace]}]).

% DHA needs to tell us: USAP, DialogueID, InvokeID, CCO-PID, OpClass, InvTimer
init([Usap, DialogueId, InvokeId, CcoPid, OpClass, InvTimeout]) ->
	State = #state{usap = Usap, dialogueId = DialogueId,
			invokeId = InvokeId, cco = CcoPid,
			op_class = OpClass, inv_timeout = InvTimeout},
	{ok, idle, State}.

%%----------------------------------------------------------------------
%%  The gen_fsm call backs
%%----------------------------------------------------------------------

%% Start the Invocation State Machine (ISM) process
%% reference: Figure A.7/Q.774 (sheet 1 of 6)

% CCO -> ISM: Operation sent
idle('operation-sent', State) ->
	% start invocation timer
	Tinv = timer:apply_after(State#state.inv_timeout, gen_fsm,
				 send_all_state_event,
				 [self(), {timer_expired, invoke}]),
	case State#state.op_class of
		1 ->
			StateName = op_sent_cl1;
		2 ->
			StateName = op_sent_cl2;
		3 ->
			StateName = op_sent_cl3;
		4 ->
			StateName = op_sent_cl4
	end,
	{next_state, StateName, State#state{inv_timer = Tinv}}.

op_sent_cl1(P=#'TC-RESULT-L'{}, State) ->
	% Figure A.7/Q.774 (2 of 6)
	% TC-RESULT-L.ind to user
	gen_fsm:send_event(State#state.usap, P),
	% stop invocation timer
	timer:cancel(State#state.inv_timer),
	% start reject timer
	Trej = start_reject_timer(),
	{next_state, wait_for_reject, State#state{rej_timer = Trej}};
op_sent_cl1(P=#'TC-U-REJECT'{}, State) ->
	% Figure A.7/Q.774 (2 of 6)
	% TC-U-ERROR.ind to user
	gen_fsm:send_event(State#state.usap, P),
	% stop invocation timer
	timer:cancel(State#state.inv_timer),
	% start reject timer
	Trej = start_reject_timer(),
	{next_state, wait_for_reject, State#state{rej_timer = Trej}};
op_sent_cl1(P=#'TC-RESULT-NL'{}, State) ->
	% Figure A.7/Q.774 (2 of 6)
	% TC-RESULT-NL.ind to user
	gen_fsm:send_event(State#state.usap, P),
	{next_state, op_sent_cl1, State};
op_sent_cl1('terminate', State) ->
	% stop invocation timer
	timer:cancel(State#state.inv_timer),
	{stop, normal, State}.

wait_for_reject('terminate', State) ->
	% stop reject timer
	timer:cancel(State#state.rej_timer),
	{stop, normal, State};
wait_for_reject({timer_expired, reject}, State) ->
	% reject timer expiry
	% terminate
	{stop, rej_timer_exp, State}.

op_sent_cl2(#'TC-U-REJECT'{}, State) ->
	% FIXME: TC-U-ERROR.ind to user
	% stop invocation timer
	timer:cancel(State#state.inv_timer),
	% start reject timer
	Trej = start_reject_timer(),
	{next_state, wait_for_reject, State#state{rej_timer = Trej}};
op_sent_cl2(Op, State) when
			is_record(Op, 'TC-RESULT-L');
			is_record(Op, 'TC-RESULT-NL') ->
	% Generate REJ component to CCO
	Problem = {'ReturnResultProblem', resultResponseUnexpected},
	Reject = #'TC-R-REJECT'{dialogueID = State#state.dialogueId,
				invokeID = State#state.invokeId,
				problemCode = Problem},
	gen_server:cast(State#state.cco, {reject_component, Reject}),
	% stop invocation timer
	timer:cancel(State#state.inv_timer),
	% terminate
	{stop, class2_result, State};
op_sent_cl2('terminate', State) ->
	% terminate
	{stop, normal, State}.

op_sent_cl3(P=#'TC-RESULT-L'{}, State) ->
	% Figure A.7/Q.774 (5 of 6)
	% TC-RESULT-L.ind to user
	gen_fsm:send_event(State#state.usap, P),
	% stop invocation timer
	timer:cancel(State#state.inv_timer),
	% start reject timer
	Trej = start_reject_timer(),
	{next_state, wait_for_reject, State#state{rej_timer = Trej}};
op_sent_cl3(P=#'TC-RESULT-NL'{}, State) ->
	% TC-RESULT-NL.ind to user
	gen_fsm:send_event(State#state.usap, P),
	{next_state, op_sent_cl3, State};
op_sent_cl3('terminate', State) ->
	% stop invocation timter
	timer:cancel(State#state.inv_timer),
	% terminate
	{stop, normal, State}.

op_sent_cl4('terminate', State) ->
	% terminate
	{stop, normal, State};
op_sent_cl4(Op, State) ->
	% Figure A.7/Q.774 (6 of 6)
	% generate REJ component to CCO
	Problem = {'ReturnResultProblem', resultResponseUnexpected},
	Reject = #'TC-R-REJECT'{dialogueID = State#state.dialogueId,
				invokeID = State#state.invokeId,
				problemCode = Problem},
	gen_server:cast(State#state.cco, {reject_component, Reject}),
	% stop invocation timer
	timer:cancel(State#state.inv_timer),
	% terminate
	{stop, cl4_op_received, State}.

handle_event({timer_expired, invoke}, _StateName, State) ->
	% invocation timer expiry
	#state{dialogueId = DlgId, invokeId = InvId} = State,
	% TC-L-CANCEL.ind to user
	P = #'TC-L-CANCEL'{dialogueID = DlgId, invokeID = InvId},
	gen_fsm:send_event(State#state.usap, P),
	{stop, inv_timer_expired, State}.

%% handle any other message
handle_info(Info, StateName, State) ->
	error_logger:format("~w (~w) received unexpected message: ~w~n", [?MODULE, self(), Info]),
	{next_state, StateName, State}.

%% handle a shutdown request
terminate(_Reason, _StateName, _State) -> ok.

%% handle updating state data due to a code replacement
code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.


start_reject_timer() ->
	timer:apply_after(?REJECT_TIMER, gen_fsm,
			  send_event,
			  [self(), {timer_expired, reject}]).
