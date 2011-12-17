%%% $Id: tcap_tco_server.erl,v 1.7 2005/08/04 09:33:17 vances Exp $
%%%---------------------------------------------------------------------
%%% @copyright 2004-2005 Motivity Telecom
%%% @author Vance Shipley <vances@motivity.ca> [http://www.motivity.ca]
%%% @end
%%%
%%% Copyright (c) 2004-2005, Motivity Telecom
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
%%% @doc Transaction Coordinator (TCO) functional block within the
%%% 		transaction sub-layer of ITU TCAP.
%%%
%%% 	<p>This module implements the transaction coordinator (TCO)
%%% 	functional block.  Adaptations to specific SCCP layer 
%%% 	and TC-User implementations may be implemented as callback
%%% 	modules behaving to this behaviour module.  This module behaves
%%% 	to <tt>gen_server</tt>.</p>
%%%
%%% 	<h2>Usage</h2>
%%% 	<p>The callback module should be implemented as a gen_server
%%% 	behaviour but with a <tt>tcap_tco_server</tt> behaviour
%%% 	module attribute:
%%% 	<pre>
%%% 	-behaviour(tcap_tco_server).</pre></p>
%%%
%%% 	<p>The call back module handles the SCCP -&gt; TCAP primitives
%%% 	directly, performs any reformatting required, and returns the
%%% 	standard primitives to the <tt>tcap_tco_server</tt> handler.  A
%%% 	very simple example is the 
%%% 	<a href="http://www.motivity.ca/sccp"><tt>sccp</tt></a>
%%% 	application which implements the SCCP SAP as a pid() which sends
%%%   and receives messages in the primitive format.  In our callback
%%% 	module we create a <tt>handle_info/2</tt> clause which matches
%%% 	the primitives:</p>
%%%
%%% 	<p><pre>handle_info({'N', _, indication, _} = Primitive, State) -&gt;
%%% 	      {primitive, Primitive, State}.</pre>
%%% 	As the example above illustrates the <tt>tcap_tco_server</tt>
%%% 	behaviour extends the allowed return values to accept the direct
%%% 	return of a received service primitive.</p>
%%%
%%% 	<p>The <tt>handle_cast/2</tt> function may be used in the same
%%% 	way.</p>
%%%
%%% 	<h2><a name="calbacks">Callback Functions</a></h2>
%%%
%%% 	<p>In addition to the <tt>gen_server</tt> callbacks the following
%%% 	callback functions are used.</p>
%%% 	
%%% 	<h3><a name="send_primitive-2">send_primitive/2</a></h3>
%%%
%%% 	<p><tt>send_primitive(Primitive, State) -&gt; void()</tt>
%%% 	<ul><li><tt>Primitive = {'N', 'UNITDATA', request, UdataParams}</tt></li>
%%% 		<li><tt>UdataParams = #'N-UNITDATA'{}</tt></li>
%%% 	</ul>
%%% 	The TCO will call this function when it has a service primitive
%%% 	to deliver to the SCCP layer.</p>
%%%
%%% 	<h3><a name="start_user-2">start_user/2</a></h3>
%%%
%%% 	<p><tt>start_user(CSL, DialogueID, State) -&gt; pid()</tt>
%%%   <ul><li><tt>CSL = {DHA, CCO}</tt></li>
%%% 	<li><tt>DHA = pid()</tt></li>
%%% 	<li><tt>CCO = pid()</tt></li>
%%% 	<li><tt>DialogueID = tid()</tt></li>
%%% 	</ul>
%%% 	This function is called by a dialogue handler (DHA) to initialize
%%% 	a local TC-User for a dialogue begun by a remote TC-User.</p>
%%% 	<p><tt>CSL</tt> is the component sublayer identifier which 
%%% 	contains the pids of the dialogue handler and component coordinator.</p>
%%% 	<p>Returns the pid of the TC-User process whcih will handle the
%%% 	new dialogue.</p>
%%%
%%% 	<h3><a name="start_transaction-1">start_transaction/2</a></h3>
%%%
%%% 	<p><tt>start_transaction(TransactionID, State) -&gt; StartFunc</tt>
%%%   <ul><li><tt>TransactionID = tid()</tt></li>
%%% 	<li><tt>State = term()</tt></li>
%%% 	<li><tt>StartFunc = {M,F,A}</tt></li>
%%% 	<li><tt>M = F = atom()</tt></li>
%%% 	<li><tt>A = [term()]</tt></li>
%%% 	</ul>
%%% 	The callback module may optionally export this function
%%%	to overide the default method used to start a transaction
%%% 	state machine (TSM).</p>
%%% 	<p>StartFunc defines the function call used to start the TSM
%%% 	process.  It should be a module-function-arguments tuple
%%% 	<tt>{M,F,A}</tt> used as <tt>apply(M,F,A)</tt>.</p>
%%% 	<p>The start function must create and link to the child process,
%%% 	and should return <tt>{ok, Child}</tt> where <tt>Child</tt> is
%%% 	the pid of the child process.</p>
%%% 	<p>See the description of StartFunc in the supervisor module.</p>
%%%
%%% 	<h3><a name="start_dialogue-1">start_dialogue/1</a></h3>
%%%
%%% 	<p><tt>start_dialogue(DialogueID, State) -&gt; StartFunc</tt>
%%%   <ul><li><tt>DialogueID = tid()</tt></li>
%%% 	<li><tt>State = term()</tt></li>
%%% 	<li><tt>StartFunc = {M,F,A}</tt></li>
%%% 	<li><tt>M = F = atom()</tt></li>
%%% 	<li><tt>A = [term()]</tt></li>
%%% 	</ul>
%%% 	The callback module may optionally export this function
%%%	to overide the default method used to start a dialogue
%%% 	handler (DHA).</p>
%%% 	<p>StartFunc defines the function call used to start the DHA
%%% 	process.  It should be a module-function-arguments tuple
%%% 	<tt>{M,F,A}</tt> used as <tt>apply(M,F,A)</tt>.</p>
%%% 	<p>The start function must create and link to the child process,
%%% 	and should return <tt>{ok, Child}</tt> where <tt>Child</tt> is
%%% 	the pid of the child process.</p>
%%% 	<p>See the description of StartFunc in the supervisor module.</p>
%%%
%%% @end
%%%
%%% @reference ITU-T Q.774 (06/97) Annex A Transaction capabilities SDLs
%%%

-module(tcap_tco_server).
-copyright('Copyright (c) 2003-2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision: 1.7 $').

-behaviour(gen_server).

% export the gen_server interface
-export([start/4, start/5, start_link/3, start_link/4,
		call/2, call/3, multi_call/2, multi_call/3, multi_call/4,
		cast/2, abcast/2, abcast/3, reply/2, enter_loop/4, enter_loop/5]).

% export the gen_server call backs
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2]).

% behaviour modules must export this function
-export([behaviour_info/1]).

% api for other modules
-export([new_tid/0]).

%% define what callbacks users must export
%%
%% @hidden
behaviour_info(callbacks) ->
	gen_server:behaviour_info(callbacks)
	% add the tcap_tco_server required callbacks
	++ [{send_primitive, 2}, {start_transaction, 2}, {start_dialogue, 2}];
behaviour_info(Other) -> 
	gen_server:behaviour_info(Other).

-include("TCAPMessages.hrl").
-include("tcap.hrl").
-include("sccp.hrl").

-record(state, {supervisor, module, ext_state}).

%%----------------------------------------------------------------------
%%  The gen_server call backs
%%----------------------------------------------------------------------

%% @hidden
init([Sup, Module, Args]) when is_list(Args) ->
	process_flag(trap_exit, true),
	case Module:init(Args) of
		{ok, ExtState} ->
			NewState = #state{supervisor = Sup, module = Module, ext_state = ExtState},
			{ok, NewState};
		{ok, ExtState, Timeout} ->
			NewState = #state{supervisor = Sup, module = Module, ext_state = ExtState},
			{ok, NewState, Timeout};
		{stop, Reason} ->
			{stop, Reason};
		ignore ->
			ignore;
		Other ->
			Other
	end.

%% @hidden
%%
% assign a new dialogue ID
handle_call(dialogueID, From, State) ->
	{reply, new_tid(), State};
% shutdown the server
handle_call(stop, _From, State) ->
	{stop, shutdown, ok, State};
% unknown request
handle_call(Request, From, State) ->
	Module = State#state.module,
	case Module:handle_call(Request, From, State#state.ext_state) of
		{reply, Reply, ExtState} ->
			{reply, Reply, State#state{ext_state = ExtState}};
		{reply, Reply, ExtState, Timeout} ->
			{reply, Reply, State#state{ext_state = ExtState}, Timeout};
		{noreply, ExtState} ->
			{noreply, State#state{ext_state = ExtState}};
		{noreply, ExtState, Timeout} ->
			{noreply, State#state{ext_state = ExtState}, Timeout};
		{stop, Reason, Reply, ExtState} ->
			{stop, Reason, Reply, State#state{ext_state = ExtState}};
		{stop, Reason, ExtState} ->
			{stop, Reason, State#state{ext_state = ExtState}};
		Other ->
			Other
	end.

%% @spec (Request, State) -> Result
%% 	Info = term()
%% 	State = term()
%% 	Result = {noreply, NewState} | {noreply, NewState, Timeout}
%% 	         | {stop, Reason, NewState} 
%% 	         | {primitive, Primitive, NewState}
%% 	NewState = term()
%% 	Timeout = int() | infinity
%% 	Reason = term()
%% 	Primitive = {'N', 'UNITDATA', indication, SccpParams} |
%% 	            {'N', 'NOTICE', indication, SccpParams}
%%
%% @doc Receive a message sent with <tt>gen_server:cast/2</tt>.
%%
%% 	<p>A user callback module may return an SCCP service primitive
%% 	to TCO for processing with the return value 
%% 	<tt>{primitive, Primitive, NewState}</tt>.</p>
%%
%% @see //stdlib/gen_server:handle_cast/2
%%
%% @end
%%
% service primitive indications from the network layer
%
% reference: Figure A.3/Q.774 (sheet 1 of 4)
handle_cast({'N', 'UNITDATA', indication, UdataParams}, State) 
		when is_record(UdataParams, 'N-UNITDATA') ->
	case 'TR':decode('TCMessage', UdataParams#'N-UNITDATA'.userData) of
		{ok, {unidirectional, TPDU}} ->
			case 'TR':decode('Unidirectional', TPDU) of
				{ok, Unidirectional} ->
					% Create a Dialogue Handler (DHA) 
					DialogueID = new_tid(),
					StartFunc = get_start(dialogue, DialogueID, State),
					ChildSpec = {DialogueID, StartFunc, temporary, 4000, worker, [tcap_dha_fsm]},
					{ok, DHA} = supervisor:start_child(State#state.supervisor, ChildSpec),
					% TR-UNI indication CSL <- TSL
					UserData = #'TR-user-data'{dialoguePortion = Unidirectional#'Unidirectional'.dialoguePortion,
							componentPortion = Unidirectional#'Unidirectional'.components},
					TrParams = #'TR-UNI'{qos =
							destAddress = UdataParams#'N-UNITDATA'.calledAddress,
							origAddress = UdataParams#'N-UNITDATA'.callingAddress,
							userData = UserData},
					gen_fsm:send_event(DHA, {'TR', 'UNI', indication, TrParams}),
					{noreply, State};
				{error, Reason} ->
					% Discard received message
					% reference: Figure A.3/Q/774 (sheet 4 of 4) label (3)
					error_logger:error_report(["Syntax error in received N-UNI", {error, Reason},
							{caller, UdataParams#'N-UNITDATA'.callingAddress},
							{called, UdataParams#'N-UNITDATA'.calledAddress}]),
					{noreply, State}
			end;
		{ok, {'begin', TPDU}} ->
			case 'TR':decode('Begin', TPDU) of
				{ok, Begin} ->
					% Assign local transaction ID
					TransactionID = new_tid(),
					StartFunc = get_start(in_transaction, TransactionID, State),
					ChildSpec = {TransactionID, StartFunc, temporary, infinity, supervisor, [tcap_tsm_fsm]},
					% Is TID = no TID?
					% Note:  The assignment of the ID above just gets the next available
					%        value and doesn't ensure that it is not in use (unlikely)
					%        or that there are enough resources available.  The real
					%        test is in whether the start succeeds.
					case supervisor:start_child(State#state.supervisor, ChildSpec) of
						{ok, TSM} ->
							% Created a Transaction State Machine (TSM)
							TsmParams = UdataParams#'N-UNITDATA'{userData = Begin},
							% BEGIN received TSM <- TCO
							gen_fsm:send_event(TSM, {'BEGIN', received, TsmParams});
						_Other ->
							% TID = no TID
							% Build ABORT message (P-Abort Cause = Resource Limitation)
							Abort = {abort, #'Abort'{dtid = TPDU#'Begin'.otid,
									reason = {'p-abortCause', resourceLimitation}}},
							NewTPDU = list_to_binary('TCMessage':encode('TCMessage', Abort)),
							SccpParams = #'N-UNITDATA'{calledAddress = UdataParams#'N-UNITDATA'.callingAddress,
									callingAddress = UdataParams#'N-UNITDATA'.calledAddress,
									sequenceControl = false, returnOption = false, importance = none,
									userData = NewTPDU},
							% TR-UNI request TSL -> SCCP
							Module = State#state.module,
							Module:send_primitive({'N', 'UNITDATA', request, SccpParams}, State#state.ext_state),
							error_logger:error_report(["Unable to create TSM for received N-BEGIN",
									{caller, UdataParams#'N-UNITDATA'.callingAddress},
									{called, UdataParams#'N-UNITDATA'.calledAddress}])
					end,
					{noreply, State};
				{error, Reason} ->
% TODO
					% is OTID derivable?
					%    Build ABORT message with appropraite P-Abort Cause value
					%    N-UNITDATA request TSL -> SCCP
					% Discard received message
					% reference: Figure A.3/Q/774 (sheet 4 of 4) label (4)
					error_logger:error_report(["Syntax error in received N-BEGIN", {error, Reason},
									{caller, UdataParams#'N-UNITDATA'.callingAddress},
									{called, UdataParams#'N-UNITDATA'.calledAddress}]),
					{noreply, State}
			end;
		{ok, {continue, TPDU}} ->
			case 'TR':decode('Continue', TPDU) of
				{ok, Continue} ->
					% DTID assigned?
					case catch ets:lookup_element(tcap_transaction, TPDU#'Continue'.dtid, 2) of
						{error, _Reason}  ->
							error_logger:error_report(["DTID not found in received N-CONTINUE",
									{dtid, TPDU#'End'.dtid}, 
									{caller, UdataParams#'N-UNITDATA'.callingAddress},
									{called, UdataParams#'N-UNITDATA'.calledAddress}]),
% TODO
							% Build ABORT message with appropriate P-Abort Cause values
							% N-UNITDATA request TSL -> SCCP
							% Discard received message
							% reference: Figure A.3/Q/774 (sheet 4 of 4) label (4)
							{noreply, State};
						TSM ->
							TsmParams = UdataParams#'N-UNITDATA'{userData = Continue},
							% CONTINUE received TSM <- TCO
							gen_fsm:send_event(TSM, {'CONTINUE', received, TsmParams}),
							{noreply, State}
					end;
				{error, Reason} ->
% TODO
					% OTID derivable?
					% DTID assigned?
					% Build ABORT message with appropraite P-Abort Cause value
					% N-UNITDATA request TSL -> SCCP
					% Local Abort TSM <- TCO
					% Discard received message
					% reference: Figure A.3/Q/774 (sheet 4 of 4) label (2)
					error_logger:error_report(["Syntax error in received N-CONTINUE", {error, Reason},
							{caller, UdataParams#'N-UNITDATA'.callingAddress},
							{called, UdataParams#'N-UNITDATA'.calledAddress}]),
					{noreply, State}
			end;
		{ok, {'end', TPDU}} ->
			case 'TR':decode('End', TPDU) of
				{ok, End} ->
					% DTID assigned?
					case catch ets:lookup(tcap_transaction, TPDU#'End'.dtid, 2) of
						{error, _Reason}  ->
							error_logger:error_report(["DTID not found in received N-END",
									{dtid, TPDU#'End'.dtid},
									{caller, UdataParams#'N-UNITDATA'.callingAddress},
									{called, UdataParams#'N-UNITDATA'.calledAddress}]),
							% Discard received message
							% reference: Figure A.3/Q/774 (sheet 4 of 4) label (3)
							{noreply, State};
						TSM ->
							TsmParams = UdataParams#'N-UNITDATA'{userData = End},
							% END received TSM <- TCO
							gen_fsm:send_event(TSM, {'END', received, TsmParams}),
							{noreply, State}
					end;
				{error, Reason} ->
% TODO
					% DTID assigned?
					%    Local Abort TSM <- TCO
					% Discard received message
					% reference: Figure A.3/Q/774 (sheet 4 of 4) label (5)
					error_logger:error_report(["Syntax error in received N-END", {error, Reason},
							{caller, UdataParams#'N-UNITDATA'.callingAddress},
							{called, UdataParams#'N-UNITDATA'.calledAddress}]),
					{noreply, State}
			end;
		{ok, {abort, TPDU}} ->
			case 'TR':decode('Abort', TPDU) of
				{ok, Abort} ->
					% DTID assigned?
					case catch ets:lookup(tcap_transaction, TPDU#'Abort'.dtid, 2) of
						{error, _Reason} ->
							error_logger:error_report(["DTID not found in received N-ABORT",
									{dtid, TPDU#'Abort'.dtid},
									{caller, UdataParams#'N-UNITDATA'.callingAddress},
									{called, UdataParams#'N-UNITDATA'.calledAddress}]),
							% Discard received message
							% reference: Figure A.3/Q/774 (sheet 4 of 4) label (3)
							{noreply, State};
						TSM ->
							TsmParams = UdataParams#'N-UNITDATA'{userData = Abort},
							% Abort received TSM <- TCO
							gen_fsm:send_event(TSM, {'ABORT', received, TsmParams}),
							{noreply, State}
					end;
				{error, Reason} ->
% TODO
					% DTID assigned?
					%    Local Abort TSM <- TCO
					% Discard received message
					% reference: Figure A.3/Q/774 (sheet 4 of 4) label (5)
					error_logger:error_report(["Syntax error in received N-ABORT", {error, Reason},
							{caller, UdataParams#'N-UNITDATA'.callingAddress},
							{called, UdataParams#'N-UNITDATA'.calledAddress}]),
					{noreply, State}
			end;
		{ok, {error, Reason}} ->
% TODO
			% Message type unknown
			% OTID derivable?
			% DTID assigned?
			% Build ABORT message with appropraite P-Abort Cause value
			% N-UNITDATA request TSL -> SCCP
			% Local Abort TSM <- TCO
			% Discard received message
			% reference: Figure A.3/Q/774 (sheet 4 of 4) label (2)
			error_logger:error_report(["Unknown TCMessage received", {error, Reason},
							{caller, UdataParams#'N-UNITDATA'.callingAddress},
							{called, UdataParams#'N-UNITDATA'.calledAddress}]),
			{noreply, State}
	end;

handle_cast({'N', 'NOTICE', indication, NoticeParams}, State) ->
	% Extract the originating transactionID
	case 'TR':decode('TCMessage', NoticeParams#'N-NOTICE'.userData) of
		{ok, {'begin', TPDU}} ->
			case 'TR':decode('Begin', TPDU) of
				{ok, Begin} ->
					TransactionID = Begin#'Begin'.otid;
				_ ->
					TransactionID = undefined
			end;
		{ok, {continue, TPDU}} ->
			case 'TR':decode('Continue', TPDU) of
				{ok, Continue} ->
					TransactionID = Continue#'Continue'.otid;
				_ ->
					TransactionID = undefined
			end;
		_ ->
			TransactionID = undefined
	end,
	% TR-NOTICE indication CSL <- TSL
	% reference: Figure A.3/Q.774 (sheet 2 of 4)
	% The CSL is a null layer for this indication so it becomes
	% TC-NOTICE indication TCU <- TSL
	% reference: Figure A.5/Q.774 (sheet 7 of 11)
	% reference: Figure A.3/Q.774 (sheet 10 of 11)
	TcParams = #'TC-NOTICE'{
			dialogueID = TransactionID,
			origAddress = NoticeParams#'N-NOTICE'.callingAddress,
			destAddress = NoticeParams#'N-NOTICE'.calledAddress,
			reportCause = NoticeParams#'N-NOTICE'.reason},
	% TODO:  fixme!!! gen_fsm:send_event(State#state.usap, {'TC', 'NOTICE', indication, TcParams}),
	{noreply, State};


%%
%% service primitive requests from the TR-User
%% reference: Figure A.3/Q.774 (sheets 2&3 of 4)
handle_cast({'TR', 'UNI', request, UniParams}, State) 
		when is_record(UniParams, 'TR-UNI') ->
	% Assemble TR-portion of UNI message
	{SequenceControl, ReturnOption, Importance} = UniParams#'TR-UNI'.qos,
	DialoguePortion = (UniParams#'TR-UNI'.userData)#'TR-user-data'.dialoguePortion,
	ComponentPortion = (UniParams#'TR-UNI'.userData)#'TR-user-data'.componentPortion,
	case 'TR':encode('TCMessage', {unidirectional, #'Unidirectional'{
			 dialoguePortion = DialoguePortion, components = ComponentPortion}}) of
		{ok, TPDU} ->
			TpduBin = iolist_to_binary(TPDU),
			SccpParams = #'N-UNITDATA'{calledAddress = UniParams#'TR-UNI'.destAddress,
					callingAddress =  UniParams#'TR-UNI'.origAddress,
					sequenceControl = SequenceControl, returnOption = ReturnOption,
					importance = Importance, userData = TpduBin},
			Module = State#state.module,
			Module:send_primitive({'N', 'UNITDATA', request, SccpParams}, State#state.ext_state),
			{noreply, State};
		{error, Err} ->
			error_logger:error_report(["Error generating ASN1", {error, Err},
					{dialogue_portion, DialoguePortion},
					{components, ComponentPortion}]),
			{noreply, State}
	end;
handle_cast({'TR', 'BEGIN', request, BeginParams}, State) 
		when is_record(BeginParams, 'TR-BEGIN') ->
	% Create a Transaction State Machine (TSM)
	OTID = BeginParams#'TR-BEGIN'.transactionID,
	ChildName = list_to_atom("tsm_" ++ integer_to_list(OTID)),
	%%%% FIXME {ok, {M, F, A, Mods}} = application:get_env(start_tsm),
	StartFunc = get_start(out_transaction, OTID, State),
	ChildSpec = {ChildName, StartFunc, temporary, 1000, worker, [tcap_tsm_fsm]},
	{ok, TSM} = supervisor:start_child(State#state.supervisor, ChildSpec),
	gen_fsm:send_event(TSM, {'BEGIN', transaction, BeginParams}),
	{noreply, State};
handle_cast({'TR', 'CONTINUE', request, ContParams}, State)
		when is_record(ContParams, 'TR-CONTINUE') ->
	TransactionID = ContParams#'TR-CONTINUE'.transactionID,
	TSM  = ets:lookup_element(tcap_transaction, TransactionID, 2),
	gen_fsm:send_event(TSM, {'CONTINUE', transaction, ContParams}),
	{noreply, State};
handle_cast({'TR', 'END', request, EndParams}, State)
		when is_record(EndParams, 'TR-END') ->
	TransactionID = EndParams#'TR-END'.transactionID,
	TSM  = ets:lookup_element(tcap_transaction, TransactionID, 2),
	gen_fsm:send_event(TSM, {'END', transaction, EndParams}),
	{noreply, State};
handle_cast({'TR', 'U-ABORT', request, AbortParams}, State)
		when is_record(AbortParams, 'TR-U-ABORT') ->
	TransactionID = AbortParams#'TR-U-ABORT'.transactionID,
	TSM  = ets:lookup_element(tcap_transaction, TransactionID, 2),
	gen_fsm:send_event(TSM, {'ABORT', transaction, AbortParams}),
	{noreply, State};
	
%
% The TSM sends us a message as it's last action so
% we can remove the supervisor child specification
%
handle_cast({'tsm-stopped', SupRef}, State) ->
	supervisor:delete_child(State#state.supervisor, SupRef),
	% reference: Figure A.3/Q/774 (sheet 2 of 4)
	{noreply, State};

% unrecognized request
handle_cast(Request, State) ->
	Module = State#state.module,
	case Module:handle_cast(Request, State#state.ext_state) of
		{noreply, ExtState} ->
			{noreply, State#state{ext_state = ExtState}};
		{noreply, ExtState, Timeout} ->
			{noreply, State#state{ext_state = ExtState}, Timeout};
		{primitive, Primitive, ExtState} ->
			handle_cast(Primitive, State#state{ext_state = ExtState});
		{stop, Reason, ExtState} ->
			{stop, Reason, State#state{ext_state = ExtState}};
		Other ->
			Other
	end.

%% @spec (Info, State) -> Result
%% 	Info = timeout | term()
%% 	State = term()
%% 	Result = {noreply, NewState} | {noreply, NewState, Timeout}
%% 	         | {stop, Reason, NewState} 
%% 	         | {primitive, Primitive, NewState}
%% 	NewState = term()
%% 	Timeout = int() | infinity
%% 	Reason = term()
%% 	Primitive = {'N', 'UNITDATA', indication, SccpParams} |
%% 	            {'N', 'NOTICE', indication, SccpParams}
%%
%% @doc Receive a message sent with '!'.
%%
%% 	<p>A user callback module may return an SCCP service primitive
%% 	to TCO for processing with the return value 
%% 	<tt>{primitive, Primitive, NewState}</tt>.</p>
%%
%% @see //stdlib/gen_server:handle_info/2
%%
handle_info({'EXIT', _Pid, Reason}, State) ->
	{stop, Reason, State};
handle_info(Info, State) ->
	Module = State#state.module,
	case Module:handle_info(Info, State#state.ext_state) of
		{noreply, ExtState} ->
			{noreply, State#state{ext_state = ExtState}};
		{noreply, ExtState, Timeout} ->
			{noreply, State#state{ext_state = ExtState}, Timeout};
		{primitive, Primitive, ExtState} ->
			handle_cast(Primitive, State#state{ext_state = ExtState});
		{stop, Reason, ExtState} ->
			{stop, Reason, State#state{ext_state = ExtState}};
		Other ->
			Other
	end.

%% @hidden
terminate(Reason, State) ->
	Module = State#state.module,
	Module:terminate(Reason, State#state.ext_state).

%% @hidden
code_change(OldVersion, statename, State, Extra) ->
	Module = State#state.module,
	case Module:code_change(OldVersion, State#state.ext_state, Extra) of
		{ok, ExtState} ->
			{ok, State#state{ext_state = ExtState}};
		Other ->
			Other
	end.

%%----------------------------------------------------------------------
%% internal functions
%%----------------------------------------------------------------------

%% @hidden
%%
%% get the next originating transaction id from the global counter
%%
%% NOTE:  we are simply assuming that when the counter rolls over the last 
%%        transaction to have this ID is long gone (4.2 billion IDs)
%%
%% reference: Figure A.3 bis/Q.774
new_tid() ->
	ets:update_counter(tcap_transaction, transactionID, {2, 1, 16#ffffffff, 0}).

get_start(dialogue, DialogueID, State) ->
	Module = State#state.module,
	case erlang:function_exported(Module, start_dialogue, 1) of
		true ->
			Module:start_dialogue(DialogueID, State#state.ext_state);
		false ->
			StartUserFun = fun(CSL) -> Module:start_user(CSL, DialogueID, State#state.ext_state) end,
			StartArgs = [DialogueID, self(), StartUserFun],
			{gen_fsm, start_link, [tcap_dha_fsm, StartArgs, []]}
	end;
get_start(in_transaction, TransactionID, State) ->
	Module = State#state.module,
	case erlang:function_exported(Module, start_transaction, 1) of
		true ->
			Module:start_transaction(TransactionID, State#state.ext_state);
		false ->
			SendFun = fun(P) -> Module:send_primitive(P, State#state.ext_state) end,
			StartDHA = get_start(dialogue, TransactionID, State),
			StartArgs = [TransactionID, SendFun, StartDHA],
			{gen_fsm, start_link, [tcap_dha_fsm, StartArgs, []]}
	end;
get_start(out_transaction, TransactionID, State) ->
	Module = State#state.module,
	case erlang:function_exported(Module, start_transaction, 1) of
		true ->
			Module:start_transaction(TransactionID, State#state.ext_state);
		false ->
			SendFun = fun(P) -> Module:send_primitive(P, State#state.ext_state) end,
			StartDHA = get_start(dialogue, TransactionID, State),
			StartArgs = [TransactionID, SendFun, StartDHA],
			{gen_fsm, start_link, [tcap_dha_fsm, StartArgs, []]}
	end.

%%----------------------------------------------------------------------
%%  The gen_server API functions
%%----------------------------------------------------------------------

%% @hidden
start(Module, SupRef, Args, Options) ->
	gen_server:start(?MODULE, [SupRef, Module, Args], Options).

%% @hidden
start(ServerRef, SupRef, Module, Args, Options) ->
	gen_server:start(ServerRef, ?MODULE, [SupRef, Module, Args], Options).

%% @hidden
start_link(Module, Args, Options) ->
	gen_fsm:start_link(?MODULE, [Module, Args], Options).

%% @hidden
start_link(ServerRef, Module, Args, Options) ->
	gen_fsm:start_link(ServerRef, ?MODULE, [Module, Args], Options).

%% @hidden
call(ServerRef, Request) ->
	gen_server:call(ServerRef, Request).

%% @hidden
call(ServerRef, Request, Timeout) ->
	gen_server:call(ServerRef, Request, Timeout).

%% @hidden
multi_call(Name, Request) ->
	gen_server:multi_call(Name, Request).

%% @hidden
multi_call(Nodes, Name, Request) ->
	gen_server:multi_call(Nodes, Name, Request).

%% @hidden
multi_call(Nodes, Name, Request, Timeout) ->
	gen_server:multi_call(Nodes, Name, Request, Timeout).

%% @hidden
cast(ServerRef, Request) ->
	gen_server:cast(ServerRef, Request).

%% @hidden
abcast(Name, Request) ->
	gen_server:abcast(Name, Request).

%% @hidden
abcast(Nodes, Name, Request) ->
	gen_server:abcast(Nodes, Name, Request).

%% @hidden
reply(Client, Reply) ->
	gen_server:reply(Client, Reply).

%% @hidden
enter_loop(Module, Options, State, ServerName, Timeout) ->
	gen_server:enter_loop(Module, Options, State, ServerName, Timeout).

%% @hidden
enter_loop(Module, Options, State, Timeout) ->
	gen_server:enter_loop(Module, Options, State, Timeout).
% enter_loop(Module, Options, State, ServerName) ->
%	gen_server:enter_loop(Module, Options, State, ServerName).

