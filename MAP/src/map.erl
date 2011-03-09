%%% $Id: map.erl,v 1.1 2005/02/13 00:42:12 vances Exp $
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
%%% @doc Mobile Application Part
%%%	<p>This module implements the user's API to the MAP protocol
%%% 	stack application.</p>
%%%
%%% @reference <a href="index.html">MAP User's Guide</a>
%%%
         
-module(map).
-copyright('Copyright (c) 2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision: 1.1 $').

%% our published API functions
-export([start/0, stop/0]).
-export([open/3, close/1]).
-export([start_dialogue/3]).


%% @type map_options(). MAP layer options
%% 	<p>A list of one or more of the following tuples.</p>
%% 	<dl>
%%			<dt><tt>{variant, Variant}</tt></dt><dd><tt>gsm | ansi</tt></dd>
%% 	</dl>
%% @end

%%----------------------------------------------------------------------
%%  The API functions
%%----------------------------------------------------------------------

%% @spec () -> Result
%% 	Result = ok | {error, Reason}
%% 	Reason = term()
%%
%% @equiv application:start(map)
%%
start() ->
	application:start(map).

%% @spec () -> Result
%% 	Result = ok | {error, Reason}
%% 	Reason = term()
%%
%% @equiv application:stop(map)
%%
stop() ->
	application:stop(map).

%% @spec (Module::atom(), Args::term(), Options::term()) -> PM
%% 	Options = [map_options() | GenFsmOptions]
%% 	GenFsmOptions = list()
%% 	PM = pid()
%%
%% @doc Start a new MAP layer service access point.
%% 	<p>A new MAP protocol machine (PM) is created.  The PM is
%% 	a collection of service state machines (SSM), one per MAP specific
%% 	service invoked, coordinated by a single dialogue state machine 
%% 	(DSM).  There are two types of SSM; requesting service state
%% 	machines (RSM) and performing service state machines (PSM).</p>
%%
%% 	<p><tt>Module</tt> is the name of a
%% 	<a href="map_dsm_fsm.html"><tt>map_dsm_fsm</tt></a>
%% 	behaviour callback module which will provide the TCAP layer
%% 	adaptation.</p>
%%
%% 	<p><tt>Args</tt> has meaning only to the callback module but 
%% 	presumably would identify the specific TCAP service access 
%% 	point (i.e. subsystem number) for this MAP layer.</p>
%%
%% 	<p><tt>Options</tt> may include
%% 	<a href="#map_options">map_options()</a> and <tt>gen_fsm</tt>
%% 	options passed to the callback module.</p>
%%
%% 	<p>The callback module will be started with:<br/>
%% 		<tt>gen_fsm:start_link(Module, Args, GenFsmOptions)</tt>
%% 	</p>
%%
%% 	<p>Returns the pid of the DSM.</p>
%%
%% @see gen_server:start_link/3
%%
open(Module, Args, Options) ->
	{ok, SupRef} = application:get_env(map, supref),
	case supervisor:start_child(SupRef, [Module, Args, Options]) of
		{ok, Sup} ->
			Children = supervisor:which_children(Sup),
			{value, {dsm, DSM, _, _}} = lists:keysearch(dsm, 1, Children),
			DSM;
		{error, Reason} ->
			exit(Reason)
	end.

%% @spec (PM::pid()) -> ok
%%
%% @doc Close a MAP service access point.
%%
%% 	<p><tt>PM</tt> is the pid of a DSM returned in a previous call to 
%% 	<a href="#open-3"><tt>open/3</tt></a>.</p>
%%
close(PM) when is_pid(PM) ->
	gen_server:call(PM, close).

