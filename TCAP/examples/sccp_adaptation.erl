%%% $Id: sccp_adaptation.erl,v 1.1 2005/02/11 03:32:45 vances Exp $
%%%---------------------------------------------------------------------
%%% @author Vance Shipley <vances@motivity.ca>
%%%
%%% @doc Example Transaction Coordinator (TCO) adaptation callback module.
%%%
%%% @private
%%%

-module(tcap_example_adaptation).
-author('vances@motivity.ca').
-vsn('$Revision: 1.1 $').

-behaviour(tcap_tco_server).

% export the gen_server call backs
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2, code_change/3]).

% export the tcap_tco_server specific call backs
-export([send_primitive/2, start_transaction/2, start_dialogue/2]).

-record(state, {nsap}).

%%----------------------------------------------------------------------
%%  The tcap_tco_server specific call backs
%%----------------------------------------------------------------------

%% @spec (Primitive, State) -> void()
%% 	Primitive = {'N', 'UNITDATA', request, UdataParams}
%%
%% @doc Deliver service primitive to the SCCP layer.
%%
send_primitive(Primitive, State) ->
	State#state.nsap ! Primitive.

%% @spec (DialogueID, CSL, State) -> pid()
%% 	DialogueID = int()
%%
%% @doc Start a MAP dialogue state machine (DSM) TC-User process.
%%
start_user(DialogueID, CSL, State) ->
	map:open(State#state.map, DialogueID, CSL).


%%----------------------------------------------------------------------
%%  The gen_server call backs
%%----------------------------------------------------------------------

%% @spec (Args) -> Result.
%% 	Args = [NSAP:int()]
%% 	Result = {ok, State}
%%
%% @doc Initialize the Transaction Coordinator (TCO).
%%
init([NSAP]) ->
	case sccp:start_link(self(), NSAP) of
		{ok, NSAP} ->
			case map:start_link(self(), NSAP) of
				{ok, MAP} ->
					{ok, #state{nsap = NSAP, map = MAP}}.
				Error ->
					Error
			end;
		Error ->
			Error
	end.

%% @hidden
%% 
handle_call(_Event, _From, State) ->
	{noreply, State}.

%% @spec (Primitive, State) -> Result.
%% 	Primitive = {'N', GenericName, indication, Parameter}
%% 	GenericName = 'UNITDATA' | 'NOTICE'
%% 	Parameter = #'UNITDATA'{} | #'NOTICE'{}
%% 	State = #state{}
%%
%% @doc Handle a service primitive received from the SCCP SAP.
%%
handle_info({'N', _, indication, _} = Primitive, State) ->
	{primitive, Primitive, State}.
	
%% @hidden
%% 
handle_cast(_Event, State) ->
	{noreply, State};

%% @hidden
terminate(_Reason, _State) ->
	ok.

%% @hidden
code_change(_OldVersion, State, _Extra) ->
	{ok, State}.


%%----------------------------------------------------------------------
%% internal functions
%%----------------------------------------------------------------------

