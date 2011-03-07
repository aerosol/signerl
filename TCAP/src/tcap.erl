%%% $Id: tcap.erl,v 1.10 2011/03/07 17:21:09 vances Exp $
%%%---------------------------------------------------------------------
%%% @copyright 2004-2005 Motivity Telecom Inc.
%%% @author Vance Shipley <vances@motivity.ca> [http://www.motivity.ca]
%%% @end
%%%
%%% Copyright Motivity Telecom Inc. 2004-2005
%%%
%%% All rights reserved. No part of this computer program(s) may be
%%% used, reproduced, stored in any retrieval system, or transmitted,
%%% in any form or by any means, electronic, mechanical, photocopying,
%%% recording, or otherwise without prior written permission of
%%% Motivity Telecom Inc.
%%%---------------------------------------------------------------------
%%%
%%% @doc Transaction Capabilities Application Part
%%%	<p>This module implements the user's API to the TCAP protocol
%%% 	stack application.Transaction Capabilities are </p>
%%%
%%% @reference <a href="index.html">TCAP User's Guide</a>
%%%
         
-module(tcap).
-copyright('Copyright (c) 2004-2005 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision: 1.10 $').

%% our published API functions
-export([start/0, stop/0]).
-export([open/3, close/1]).
%-export([start_tsl/3]).
-export([dialogueID/1, transactionID/1]).


%% @type tcap_options(). TCAP layer options
%% 	<p>A list of one or more of the following tuples.</p>
%% 	<dl>
%%			<dt><tt>{variant, Variant}</tt></dt><dd><tt>itu | ansi</tt></dd>
%% 	</dl>
%% @end

%%----------------------------------------------------------------------
%%  The API functions
%%----------------------------------------------------------------------

%% @spec () -> Result
%% 	Result = ok | {error, Reason}
%% 	Reason = term()
%%
%% @equiv application:start(tcap)
%%
start() ->
	application:start(tcap).

%% @spec () -> Result
%% 	Result = ok | {error, Reason}
%% 	Reason = term()
%%
%% @equiv application:stop(tcap)
%%
stop() ->
	application:stop(tcap).

%% @spec (TSL::pid(), TCU::pid(), Args) -> CSL
%% 	Args = [term()]
%% 	CSL = {DHA, CCO}
%% 	DHA = pid()
%% 	CCO = pid()
%%
%% @doc Start a new component sublayer (CSL).
%% 	<p>Called by the TC-User to initialize the TCAP layer for a new
%% 	dialogue.</p>
%%
%% 	<p><tt>TSL</tt> is the pid returned from a previous call to
%% 	<a href="#open-3"><tt>open/3</tt></a>.</p>
%%
%% 	<p><tt>TCU</tt> is the pid of the TC-User.</p>
%%
%% 	<p>Returns <tt>{DHA, CCO}</tt>; the pids of the dialogue handler
%% 	and component coordinator in the component sublayer.</p>.
%%
open(TSL, TCU, Args) ->
	gen_server:call(TSL, {start_dialogue, TCU, Args}).

%% @spec (TSL::pid()) -> ok
%%
%% @doc Close a TCAP service layer.
%%
%% 	<p><tt>TSL</tt> is the pid returned in a previous call to 
%% 	<a href="#open-3"><tt>open/3</tt></a>.</p>
%%
close(TSL) when is_pid(TSL) ->
	gen_server:call(TSL, close).

%% @spec (TSL::pid()) -> tid()
%%
%% @doc Assign a new dialogue ID.
%%
%% 	<p><tt>TSL</tt> is the pid returned in a previous call to
%% 	<a href="#open-3"><tt>open/3</tt></a>.</p>
%%
dialogueID(TSL) when is_pid(TSL) ->
	gen_server:call(TSL, dialogueID).
	
%% @equiv dialogueID/0
%%
transactionID(TSL) when is_pid(TSL) ->
	dialogueID(TSL).

