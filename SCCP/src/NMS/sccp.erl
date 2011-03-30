%%% $Id: sccp.erl,v 1.26 2007/08/15 11:02:50 vances Exp $
%%%---------------------------------------------------------------------
%%% @copyright 2001-2005 Motivity Telecom
%%% @author Vance Shipley <vances@motivity.ca> [http://www.motivity.ca]
%%% @end
%%%
%%% Copyright (c) 2001-2005, Motivity Telecom
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
%%% @doc Signaling Connection Control Part (SCCP) application using NMS.
%%% 	<p>Implements SCCP service access points (SAP) on top of the
%%% 	<tt>nms</tt> application.</p>
%%%
%%% @see //nms
%%% @reference 
%%% 	<a href="http://www.nmscommunications.com/swDocs/Docs/6467-32/default.htm">
%%% 	NMS SCCP Developer's Reference Manual</a>
%%% @reference ITU-T Q.771-Q.774
%%% @reference ANSI T1.112
         
-module(sccp).
-copyright('Copyright (c) 2001-2007 Motivity Telecom Inc.').
-author('vances@motivity.ca').
-vsn('$Revision: 1.26 $').

-include("nms_sccp.hrl").

-behaviour(gen_server).

%% call backs needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

%% our published API functions
-export([start_link/2, start_link/3, start_link/7, stop/1]).
-export([address/1, address_itu/1, address_ansi/1, bcd_digits/1, btoi/1]).

-record(state, {user, board, entityid, serviceuserid, sap, ssn, 
			queue, context, object, port}).

%%  TODO: find a way to have autoconf define these
-define(SW_INT, 1).
-define(SW_ANSI, 2).
-define(ENC_UNKNOWN, 0).
-define(ENC_BCD_ODD, 1).
-define(ENC_BCD_EVEN, 2).

%%----------------------------------------------------------------------
%%  The sccp exported API (not the service primitives)
%%----------------------------------------------------------------------

%% @spec (USAP::pid(), SSN::integer()) -> {ok, NSAP} | {error, Reason}
%% 	NSAP = pid()
%% 	Reason = term()
%%
%% @doc Starts an sccp server.
%% 	<p><tt>USAP</tt> is the pid of the SCCP-User.</p>
%% 	<p><tt>SSN</tt> is the SCCP subsystem number for this SAP.</p>
%% 	<p><tt>NSAP</tt> is a pid which the SCCP-User will use as the
%% 	service access point for the SCCP-Service.</p>
%%
start_link(User, SSN) ->
	start_link(User, 0, SSN).
%%
%% @spec (USAP::pid(), SpID::integer(), SSN::integer()) -> {ok, NSAP} | {error, Reason}
%% 	NSAP = pid()
%% 	Reason = term()
%%
%% @doc Starts an sccp server.
%% 	<p><tt>USAP</tt> is the pid of the SCCP-User.</p>
%% 	<p><tt>SpID</tt> NMS SCCP service access point ID on which to bind.
%% 	This is SCCP SAP number which is defined in the TX board SCCP
%% 	configuration.</p>
%% 	<p><tt>SSN</tt> is the SCCP subsystem number for this SAP.</p>
%% 	<p><tt>NSAP</tt> is a pid which the SCCP-User will use as the
%% 	service access point for the SCCP-Service.</p>
%%
start_link(User, SpID, SSN) ->
	start_link(User, 1, 16#20, 0, SpID, SSN, 128).
%%
%% @spec (USAP::pid(), Board::integer(), EntityID::integer(), SuID::integer(),
%% 		SpID::integer(), SSN::integer(), PoolSize::integer()) -> {ok, NSAP} | {error, Reason}
%% 	NSAP = pid()
%% 	Reason = term()
%%
%% @doc Starts an sccp server.
%% 	<p><tt>USAP</tt> is the pid of the SCCP-User.</p>
%% 	<p><tt>Board</tt> is the number of the TX board for this SAP.</p>
%% 	<p><tt>EntityID</tt> is the Entity ID which identifies this application
%% 	to the NMS TX board.</p>
%% 	<p><tt>SuID</tt> NMS SCCP calling application service user ID.
%% 	This is User SAP number which is defined in the TX board SCCP
%% 	configuration.</p>
%% 	<p><tt>SpID</tt> NMS SCCP service access point ID on which to bind.
%% 	This is SCCP SAP number which is defined in the TX board SCCP
%% 	configuration.</p>
%% 	<p><tt>SSN</tt> is the SCCP subsystem number for this SAP.</p>
%% 	<p><tt>NSAP</tt> is a pid which the SCCP-User will use as the
%% 	service access point for the SCCP-Service.</p>
%% 	<p><tt>PoolSize</tt> is the number of messages allowed to
%% 	be queued to the TX board.</p>
%%
start_link(User, Board, EntityID, SuID, SpID, SSN, PoolSize) ->
	Ourname = list_to_atom("sccp_ssn" ++ integer_to_list(SSN)),
	gen_server:start_link({local, Ourname}, ?MODULE,
			[User, Board, EntityID, SuID, SpID, SSN, PoolSize], []).

%% @spec (NSAP) -> ok
%% 	NSAP = pid()
%%
%% @doc Stop an sccp server.
%% 	<p>Closes an SCCP service access point (SAP).</p>
%% 	<p><tt>NSAP</tt> is a pid returned from a previous call to
%% 	<tt>start_link/2,3,7</tt>.</p>
%%
stop(NSAP) ->
	gen_server:call(NSAP, stop).

%% @type party().  SCCP called/calling party address.
%% 	<p>A binary() or an <tt>#'SccpAddr'{}</tt> record.</p>

%% @spec (SccpAddress::party()) -> SccpAddress
%% 	SccpAddress = party()
%%
%% @doc Encodes/decodes SCCP called/calling party addresses.
%% 	<p>Naively, but convienently, assumes national addresses
%% 	are in ANSI format and international addresses are in
%% 	ITU format when decoding.</p>
%% @end
%%
% International Indicator
address(<<0:1, _:7, _Rest/binary>> = OrigBin) ->
	address_itu(OrigBin);
% National Indicator
address(<<1:1, _:7, _Rest/binary>> = OrigBin) ->
	address_ansi(OrigBin);
% encode ANSI address records
address(CP) when is_record(CP, 'SccpAddr'), CP#'SccpAddr'.swtype == ?SW_ANSI ->
	address_ansi(CP);
% encode ITU address records
address(CP) when is_record(CP, 'SccpAddr'), CP#'SccpAddr'.swtype == ?SW_INT ->
	address_itu(CP).

%% @spec (SccpAddress::party()) -> SccpAddress
%% 	SccpAddress = party()
%%
%% @doc Encodes/decodes an ITU-T variant SCCP called/calling party address.
%% 	<p>Operates according to ITU-T Q.713 3.4.</p>
%% @end
%%
%% [note: ANSI reverse the order of the PC & SSN]
%%
address_itu(<<NatIntInd:1, RoutingInd:1, GlobalTitleInd:4, 
			SubsystemInd:1, PointCodeInd:1>>) ->
	#'SccpAddr'{presind = 1, swtype = ?SW_INT, natintind = NatIntInd,
			routingind = RoutingInd, gltitleind = GlobalTitleInd,
			subsystemind = SubsystemInd, pointcodeind = PointCodeInd};
address_itu(<<NatIntInd:1, RoutingInd:1, GlobalTitleInd:4, 
			SubsystemInd:1, PointCodeInd:1, Addresses/binary>>) ->
	case PointCodeInd of
		1 ->
			% ITU-T point codes are 14 bits (Q.713 clause 3.4.2.1)
			<<LSB:8, _:2, MSB:6, RestAddresses/binary>> = Addresses,
			PointCode = (MSB bsl 8) bor LSB,
			AddressWithPC = #'SccpAddr'{presind = 1, swtype = ?SW_INT,
					natintind = NatIntInd, pointcodeind = 1, pointcode = PointCode};
		0 ->
			RestAddresses = Addresses,
			AddressWithPC = #'SccpAddr'{presind = 1, swtype = ?SW_INT,
					natintind = NatIntInd, pointcodeind = 0}
	end,
	case SubsystemInd of
		1 ->
			<<SubSystemNumber:8, MoreAddresses/binary>> = RestAddresses,
			AddressWithSSN = AddressWithPC#'SccpAddr'{subsystemind = 1,
					subsystem = SubSystemNumber};
		0 ->
			MoreAddresses = RestAddresses,
			AddressWithSSN = AddressWithPC#'SccpAddr'{subsystemind = 0}
	end,
	case GlobalTitleInd of
		% no global title included
		2#0000 ->
			AddressWithGT = AddressWithSSN#'SccpAddr'{
					gltitleind = GlobalTitleInd};

		% global title includes nature of address indicator only
		2#0001 ->
			<<OddEven:1, NatureOfAddressInd:7, GlobalTitle/binary>> = MoreAddresses,
			case OddEven of
				0 -> Encoding = ?ENC_BCD_EVEN;
				1 -> Encoding = ?ENC_BCD_ODD
			end,
			GlobalTitleLen = size(GlobalTitle),
			GlobalTitleRestSize = (?MAX_GLT_SZ - GlobalTitleLen),
			AddressWithGT = AddressWithSSN#'SccpAddr'{
					gltitleind = GlobalTitleInd, encoding = Encoding,
					nataddrind = NatureOfAddressInd,
					gltitle = <<GlobalTitle/binary,
							<<0:GlobalTitleRestSize/native-integer-unit:8>>/binary>>,
					gltitlelen = GlobalTitleLen};

		% global title includes translation type only
		2#0010 ->
			<<TranslationType:8, GlobalTitle/binary>> = MoreAddresses,
			GlobalTitleLen = size(GlobalTitle),
			GlobalTitleRestSize = (?MAX_GLT_SZ - GlobalTitleLen),
			AddressWithGT = AddressWithSSN#'SccpAddr'{
					gltitleind = GlobalTitleInd,
					gltranstype = TranslationType,
					gltitle = <<GlobalTitle/binary,
							<<0:GlobalTitleRestSize/native-integer-unit:8>>/binary>>,
					gltitlelen = GlobalTitleLen};

		% global title includes translation type, numbering plan 
		% and encoding scheme
		2#0011 ->
			<<TranslationType:8, NumberingPlan:4, EncodingScheme:4,
					GlobalTitle/binary>> = MoreAddresses,
			GlobalTitleLen = size(GlobalTitle),
			GlobalTitleRestSize = (?MAX_GLT_SZ - GlobalTitleLen),
			AddressWithGT = AddressWithSSN#'SccpAddr'{
					gltitleind = GlobalTitleInd,
					gltranstype = TranslationType, numplan = NumberingPlan,
					encoding = EncodingScheme,
					gltitle = <<GlobalTitle/binary,
							<<0:GlobalTitleRestSize/native-integer-unit:8>>/binary>>,
					gltitlelen = GlobalTitleLen};

		% global title includes translation type, numbering plan,
		% encoding scheme and nature of address indicator
		2#0100 ->
			<<TranslationType:8, NumberingPlan:4, EncodingScheme:4,
					_:1, NatureOfAddressInd:7, GlobalTitle/binary>> = MoreAddresses,
			GlobalTitleLen = size(GlobalTitle),
			GlobalTitleRestSize = (?MAX_GLT_SZ - GlobalTitleLen),
			AddressWithGT = AddressWithSSN#'SccpAddr'{
					gltitleind = GlobalTitleInd, gltranstype = TranslationType,
					numplan = NumberingPlan, encoding = EncodingScheme,
					nataddrind = NatureOfAddressInd,
					gltitle = <<GlobalTitle/binary,
							<<0:GlobalTitleRestSize/native-integer-unit:8>>/binary>>,
					gltitlelen = GlobalTitleLen};
		% GlobalTitleInd 2#0101 to 2#0111 spare international
		%                2#1000 to 2#1110 spare national
		%                          2#1111 reserved for extension
		_ ->
			<<GlobalTitle/binary>> = MoreAddresses,
			GlobalTitleLen = size(GlobalTitle),
			GlobalTitleRestSize = (?MAX_GLT_SZ - GlobalTitleLen),
			AddressWithGT = AddressWithSSN#'SccpAddr'{
					gltitleind = GlobalTitleInd,
					gltitle = <<GlobalTitle/binary,
							<<0:GlobalTitleRestSize/native-integer-unit:8>>/binary>>,
					gltitlelen = GlobalTitleLen}
	end,
	AddressWithGT#'SccpAddr'{routingind = RoutingInd};
address_itu(CP) when is_record(CP, 'SccpAddr') ->
	Indicators = <<(CP#'SccpAddr'.natintind):1,
			(CP#'SccpAddr'.routingind):1,
			(CP#'SccpAddr'.gltitleind):4,
			(CP#'SccpAddr'.subsystemind):1,
			(CP#'SccpAddr'.pointcodeind):1>>,
	case CP#'SccpAddr'.pointcodeind of
		0 ->
			BinAfterPC = Indicators;
		1 -> 
			<<_:18, MSB:6, LSB:8>> = CP#'SccpAddr'.pointcode,
					BinAfterPC = <<Indicators/binary, LSB:8, 0:2, MSB:6>>
	end,
	case CP#'SccpAddr'.subsystemind of
		0 -> 
			BinAfterSSN = BinAfterPC;
		1 ->
			BinAfterSSN = <<BinAfterPC/binary,
					(CP#'SccpAddr'.subsystem):8>>
	end,
	GTLen = CP#'SccpAddr'.gltitlelen,
	<<GT:GTLen/binary, _/binary>> = CP#'SccpAddr'.gltitle,
	case CP#'SccpAddr'.gltitleind of
		%% no global title included 
		2#0000 ->
			BinAfterSSN;
		%% global title includes nature of address indicator only
		2#0001 ->
			case (CP#'SccpAddr'.encoding) of
				%% BCD odd number of digits
				16#01 ->
					OddEven = 1;
				%% BCD even number of digits
				16#02 ->
					OddEven = 0
			end,
			<<BinAfterSSN/binary, OddEven:1,
					(CP#'SccpAddr'.nataddrind):7, GT/binary>>;
		%% global title includes translation type only
		2#0010 ->
			<<BinAfterSSN/binary,
					(CP#'SccpAddr'.gltranstype):8, GT/binary>>;
		%% global title includes translation type, numbering plan
		%% and encoding scheme 
		2#0011 ->
			<<BinAfterSSN/binary, (CP#'SccpAddr'.gltranstype):8,
					(CP#'SccpAddr'.numplan):4, (CP#'SccpAddr'.encoding):4,
					GT/binary>>;
		%% global title includes translation type, numbering plan,
		%% encoding scheme and nature of address indicator 
		2#0100 ->
			<<BinAfterSSN/binary, (CP#'SccpAddr'.gltranstype):8,
					(CP#'SccpAddr'.numplan):4, (CP#'SccpAddr'.encoding):4,
					0:1, (CP#'SccpAddr'.nataddrind):7, GT/binary>>;
		%% spare/reserved
		_ ->
			<<BinAfterSSN/binary, GT/binary>>
	end.


%% @spec (SccpAddress::party()) -> SccpAddress
%% 	SccpAddress = party()
%%
%% @doc Encodes/decodes an ANSI variant SCCP called/calling party address.
%% 	<p>Operates according to ANSI T1.112.3.</p>
%% @end
%%
%% [note: ANSI reverse the order of the PC & SSN]
%%
address_ansi(<<NatIntInd:1, RoutingInd:1, GlobalTitleInd:4, PointCodeInd:1,
			SubsystemInd:1>>) -> 
	#'SccpAddr'{presind = 1, swtype = ?SW_ANSI, natintind = NatIntInd,
			routingind = RoutingInd, pointcodeind = PointCodeInd, 
			subsystemind = SubsystemInd, gltitleind = GlobalTitleInd};
address_ansi(<<NatIntInd:1, RoutingInd:1, GlobalTitleInd:4, 
			PointCodeInd:1, SubsystemInd:1, Addresses/binary>>) ->
	case SubsystemInd of
		1 ->
			<<SubSystemNumber:8, MoreAddresses/binary>> = Addresses,
			AddressWithSSN = #'SccpAddr'{presind = 1, swtype = ?SW_ANSI,
					natintind = NatIntInd, subsystemind = 1, subsystem = SubSystemNumber};
		0 ->
			MoreAddresses = Addresses,
			AddressWithSSN = #'SccpAddr'{presind = 1, swtype = ?SW_ANSI,
					natintind = NatIntInd, subsystemind = 0}
	end,
	case PointCodeInd of
		1 ->
			% ANSI point codes are three octets with the first octet
			% containing the network cluster member and the last 
			% containing the network identifier 
			<<Member:8, Cluster:8, Network:8,
					RestAddresses/binary>> = MoreAddresses,
			PointCode = (Network bsl 16) bor (Cluster bsl 8) bor Member,
			AddressWithPC = AddressWithSSN#'SccpAddr'{pointcodeind = 1,
					pointcode = PointCode};
		0 ->
			RestAddresses = MoreAddresses,
			AddressWithPC = AddressWithSSN#'SccpAddr'{pointcodeind = 0}
	end,
	case GlobalTitleInd of
		% no global title included
		2#0000 ->
			AddressWithGT = AddressWithPC#'SccpAddr'{
					gltitleind = GlobalTitleInd};
		% global title includes translation type, numbering plan 
		% and encoding scheme (ITU codes this format as 2#0011)
		2#0001 ->
			<<TranslationType:8, NumberingPlan:4, EncodingScheme:4,
					GlobalTitle/binary>> = RestAddresses,
			GlobalTitleLen = size(GlobalTitle),
			GlobalTitleRestSize = (?MAX_GLT_SZ - GlobalTitleLen),
			AddressWithGT = AddressWithPC#'SccpAddr'{
					gltitleind = GlobalTitleInd,
					gltranstype = TranslationType, numplan = NumberingPlan,
					encoding = EncodingScheme,
					gltitle = <<GlobalTitle/binary,
							<<0:GlobalTitleRestSize/native-integer-unit:8>>/binary>>,
					gltitlelen = GlobalTitleLen};
		% global title includes translation type only
		2#0010 ->
			<<TranslationType:8, GlobalTitle/binary>> = RestAddresses,
			GlobalTitleLen = size(GlobalTitle),
			GlobalTitleRestSize = (?MAX_GLT_SZ - GlobalTitleLen),
			AddressWithGT = AddressWithPC#'SccpAddr'{
					gltitleind = GlobalTitleInd,
					gltranstype = TranslationType,
					gltitle = <<GlobalTitle/binary,
							<<0:GlobalTitleRestSize/native-integer-unit:8>>/binary>>,
					gltitlelen = GlobalTitleLen};
		% GlobalTitleInd 2#0011 to 2#0100 not assigned for US networks
		% GlobalTitleInd 2#0101 to 2#0111 spare international
		%                2#1000 to 2#1110 spare national
		%                          2#1111 reserved for extension
		_ ->
			<<GlobalTitle/binary>> = RestAddresses,
			GlobalTitleLen = size(GlobalTitle),
			GlobalTitleRestSize = (?MAX_GLT_SZ - GlobalTitleLen),
			AddressWithGT = AddressWithPC#'SccpAddr'{
					gltitleind = GlobalTitleInd,
					gltitle = <<GlobalTitle/binary,
							<<0:GlobalTitleRestSize/native-integer-unit:8>>/binary>>,
					gltitlelen = GlobalTitleLen}
	end,
	AddressWithGT#'SccpAddr'{routingind = RoutingInd};
address_ansi(CP) when is_record(CP, 'SccpAddr') ->
	Indicators = <<(CP#'SccpAddr'.natintind):1,
			(CP#'SccpAddr'.routingind):1,
			(CP#'SccpAddr'.gltitleind):4,
			(CP#'SccpAddr'.pointcodeind):1,
			(CP#'SccpAddr'.subsystemind):1>>,
	case CP#'SccpAddr'.subsystemind of
		0 ->
			BinAfterSSN = Indicators;
		1 ->
			BinAfterSSN = <<Indicators/binary,
					(CP#'SccpAddr'.subsystem):8>>
	end,
	case CP#'SccpAddr'.pointcodeind of
		0 ->
			BinAfterPC = BinAfterSSN;
		1 ->
			Member = 16#FF band CP#'SccpAddr'.pointcode,
			Cluster = 16#FF band (CP#'SccpAddr'.pointcode bsr 8),
			Network = 16#FF band (CP#'SccpAddr'.pointcode bsr 16),
			BinAfterPC = <<BinAfterSSN/binary,
					Member:8, Cluster:8, Network:8>>
	end,
	GTLen = CP#'SccpAddr'.gltitlelen,
	<<GT:GTLen/binary, _/binary>> = CP#'SccpAddr'.gltitle,
	case CP#'SccpAddr'.gltitleind of
		% no global title
		2#0000 ->
			BinAfterPC;
		% global title includes translation type,
		% numbering plan and encoding scheme
		2#0001 ->
			<<BinAfterPC/binary, (CP#'SccpAddr'.gltranstype):8,
					(CP#'SccpAddr'.numplan):4,
					(CP#'SccpAddr'.encoding):4, GT/binary>>;
		% global title includes translation type
		2#0010 ->
			<<BinAfterPC/binary,
					(CP#'SccpAddr'.gltranstype):8, GT/binary>>;
		% spare/reserved
		_ ->
			<<BinAfterPC/binary, GT/binary>>
	end.


%% @type protoclass(). SCCP protocol class.
%% 	<p>An <tt>#'SccpProtoClass'{}</tt> record.</p>
%% @type importance().  SCCP importance.
%% 	<p>An <tt>#'SccpImportance'{}</tt> record.</p>

%% @spec (ConnectionOriented, QosParams) -> {ProtoClass, Importance}
%% 	ProtoClass = protoclass()
%% 	Importance = importance()
%%
%% @doc Creates the records required for the NMS API out of the 
%% 	qos parameter set in the service primitive.
%%
qos_parameters(ConnectionOriented, {SequenceControl, ReturnOption,
			MessagePriority}) ->
	if
		not ConnectionOriented and not SequenceControl -> Class = 0;
		not ConnectionOriented and SequenceControl -> Class = 1;
		ConnectionOriented and not SequenceControl -> Class = 2;
		ConnectionOriented and SequenceControl -> Class = 3
	end,
	case ReturnOption of
		false -> MessageHandling = 0;
		true -> MessageHandling = 8
	end,
	ProtoClass = #'SccpProtoClass'{classind = Class,
			msghandling = MessageHandling},
	case MessagePriority of
		none ->
			Importance = #'SccpImportance'{};
		Priority ->
			Importance = #'SccpImportance'{presind = 1, impvalue = Priority}
	end,
	{ProtoClass, Importance}.

%% @spec (BCDDigits) -> Digits
%% 	BCDDigits = [integer()]
%% 	Digits = [char()]
%%
%% @doc Decode BCD encoded digit string.
%% 	<p>BCD encoding uses 4 bits for each digit, packing two digits 
%% 	into each octet.</p>
%% 	<p>Returns a character string representation of the digits.</p>
%%
bcd_digits(BCDDigits) when is_binary(BCDDigits) ->
	bcd_digits(BCDDigits, []);
bcd_digits(BCDDigits) when is_list(BCDDigits) ->
	bcd_digits(list_to_binary(BCDDigits), []).
bcd_digits(<<>>, Result) -> Result;
bcd_digits(<<Second:4, First:4, Rest/binary>>, Acc) ->
	bcd_digits(Rest, Acc ++ integer_to_list(First) ++ integer_to_list(Second)).

%% @spec (BCDDigits) -> integer()
%% 	BCDDigits = [integer()]
%%
%% @doc Decode BCD encoded digit string.
%% 	<p>BCD encoding uses 4 bits for each digit, packing two digits 
%% 	into each octet.</p>
%% 	<p>Returns an integer representation of the digits.</p>
%%
btoi(BCDDigits) when is_list(BCDDigits) ->
	list_to_integer(bcd_digits(BCDDigits)).


%%----------------------------------------------------------------------
%%  The gen_server call backs
%%----------------------------------------------------------------------

%% @private
%%
%% @spec (Args) -> {ok, State}
%%
%% @doc Initialize the sccp server.
%% 	<p>This callback is caled by the new process in response to
%% 	the <tt>start_link/2,3,7</tt> functions.</p>
%%
init([User, Board, EntityID, ServiceUserID, SAP, SSN, Poolsize]) ->
	Queue = na:ctaCreateQueue(),
	Context = na:ctaCreateContext(Queue, SSN, []),
	Service = "sccp",
	ServiceManager = "sccpmgr",
	ServiceName = {Service, ServiceManager},
	ServiceAddress = 0,
	Arg = "",
	Args = [Board, 0, EntityID, 0, SAP, ServiceUserID, SSN, 0, Poolsize],
	ServiceArgs = {Arg, Args},
	MVIPAddress = {0,0,0,0,0},
	ServiceDecription = {ServiceName, ServiceAddress, ServiceArgs, MVIPAddress},
	Object = na:ctaOpenServices(Queue, Context, [ServiceDecription]),
	State = #state{user = User, board = Board, entityid = EntityID, 
			serviceuserid = ServiceUserID, sap = SAP, ssn = SSN,
			queue = Queue, context = Context, object = Object},
	% load the dynamicly linked device driver
	PrivDir = code:priv_dir(nms),
	LibDir = filename:join([PrivDir, "lib"]),
	Name = nms_sccp_drv,
	case erl_ddll:try_load(LibDir, Name, [{monitor, pending_driver}]) of
		{error, permanent} ->
			init1(State);
		{error, ErrorDescriptor} ->
			{stop, erl_ddll:format_error(ErrorDescriptor)};
		{ok, Loaded} when Loaded == loaded; Loaded == already_loaded ->
			init1(State);
		{ok, pending_driver, Ref} ->
			receive
				{'UP', Ref, driver, Name, loaded} ->
					init1(State);
				{'UP', Ref, driver, Name, permanent} ->
					init1(State);
				{'DOWN', Ref, driver, Name, load_cancelled} ->
					{stop, load_cancelled};
				{'DOWN', Ref, driver, Name, {load_failure, Failure}} ->
					{stop, erl_ddll:format_error(Failure)}
			after 10 ->
					{stop, timeout}
			end
	end.
init1(State) ->
	Port = open_port({spawn, 'nms_sccp_drv'}, [stream, binary]),
	process_flag(trap_exit, true),
	{ok, State#state{port = Port}}.

%% @private
%%
%% @spec (Request, From, State) -> {stop, shutdown, State}
%%
%% @doc Handle requests sent with <tt>gen_server:call/2,3</tt>.
%% @end
%%
% shutdown the sccp server
handle_call(stop, _From, State) ->
	{stop, shutdown, State}.
	
%% @private
%%
%% @spec (Request, State) -> {noreply, State}
%%
%% @doc Handle requests sent with <tt>gen_server:cast/2</tt>.
%% @end
%
% service primitives received from the SCCP-Users
%
% Connection-Oriented Primitives
handle_cast({'N', 'CONNECT', request, {_CalledAddress, _CallingAddress,
		_RespondingAddress, _ExpeditedDataSelection,  
		_QualityOfServiceParameterSet, _UserData, _Importance,
		_ConnectionID}}, State) ->
	{noreply, State};
handle_cast({'N', 'CONNECT', response, {_CalledAddress, _CallingAddress,
		_RespondingAddress, _ExpeditedDataSelection,  
		_QualityOfServiceParameterSet, _UserData, _Importance,
		_ConnectionID}}, State) ->
	{noreply, State};
handle_cast({'N', 'DATA', request, {_Importance, _UserData,
		_ConnectionID}}, State) ->
	{noreply, State};
handle_cast({'N', 'EXPEDITEDDATA', request , {_UserData, _ConnectionID}},
		State) ->
	{noreply, State};
handle_cast({'N', 'DISCONNECT', request, {_Originator, _Reason, _UserData,
		_RespondingAddress, _Importance, _ConnectionID}}, State) ->
	{noreply, State};
handle_cast({'N', 'RESET', request, {_Originator, _Reason,
		_ConnectionID}}, State) ->
	{noreply, State};
handle_cast({'N', 'RESET', response, {_Originator, _Reason,
		_ConnectionID}}, State) ->
	{noreply, State};
handle_cast({'N', 'INFORM', request, {_Reason, _ConnectionID,  
		_QualityOfServiceParameterSet}}, State) ->
	{noreply, State};
% Connectionless Primitives
handle_cast({'N', 'UNITDATA', request, {CalledAddress, CallingAddess,
		QualityOfServiceParameterSet, UserData}}, State) ->
	{ProtoClass, Importance} = qos_parameters(false,
			QualityOfServiceParameterSet),
	Data = #'SccpData'{presind = 1, 
			data = UserData, datalen = size(UserData)},
	SCCPUDataRqst = nms_sccp:'SccpUdataRqst'(#'SccpUdataRqst'{
			protoclass = ProtoClass,
			calledpty = address(CalledAddress),
			callingpty = address(CallingAddess),
			% TODO:  what about end-of-sequence?  (eos = 0)
			importance = Importance,
			data = Data}),
	erlang:port_command(State#state.port, <<?UDATArequest:8,
			0:24,                                   % pad for alignment
			(State#state.context):?DWORD,
			(State#state.sap):?S16, 0:16,           % pad for alignment
			SCCPUDataRqst/binary>>),
	{noreply, State};
% Management Primitives
handle_cast({'N', 'COORD', request, {_AffectedSubsystem,
		_SubsystemMultiplicityIndicator}}, State) ->
	{noreply, State};
handle_cast({'N', 'COORD', response, {_AffectedSubsystem,
		_SubsystemMultiplicityIndicator}}, State) ->
	{noreply, State};
handle_cast({'N', 'STATE', request, {_AffectedSubsystem, _UserStatus,
		_SubsystemMultiplicityIndicator}}, State) -> 
	{noreply, State}.


%% @private
%%
%% @spec (Info, State) -> {noreply, State} | {stop, Reason, State}
%%
%% @doc Handle system events and messages.
%% @end
%%
%
% service primitives received from the NMS SCCP service
%
handle_info({{sccpEventData, _Context, _Port, _UserId}, {SccpRcvInfoBlk, SccpAllMsgs}}, State) ->
	I = nms_sccp:'SccpRcvInfoBlk'(SccpRcvInfoBlk),
	MoreInfo = {I#'SccpRcvInfoBlk'.board, I#'SccpRcvInfoBlk'.evnttype,
			I#'SccpRcvInfoBlk'.suid, I#'SccpRcvInfoBlk'.connid,
			I#'SccpRcvInfoBlk'.opc},
	forward_primitive(State#state.user, I#'SccpRcvInfoBlk'.indtype,
			MoreInfo, SccpAllMsgs), 
	{noreply, State};
% trapped exit signals
handle_info({'EXIT', Port, Reason} = R, State) when is_port(Port) ->
	error_logger:error_report([Port, Reason, "Port terminated"]),
	{stop, R, State};
handle_info({'EXIT', Pid, Reason} = R, State) when is_pid(Pid) ->
	error_logger:error_report([Pid, Reason, "Linked Pid terminated"]),
	{stop, R, State}.

%% @private
%%
%% @spec (Reason, State) -> ok
%%
%% @doc Called when the system is being shutdown.
%% @end
%%
% someone wants us to shutdown and cleanup
terminate(_Reason, _State) -> ok.

%% @private
%%
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%%
%% @doc Called during a release upgarde.
%%
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%----------------------------------------------------------------------
%%  internal functions
%%----------------------------------------------------------------------

%% @private
%%
%% @spec (User, Name, Params, SccpAllMsgs) -> ok
%%
%% @doc Handles incoming primitives from the NMS driver and sends standardized
%% 	SCCP-service primitives to the service user.
%% @end
%%
%% TODO:  all parameters should be encoded in standardized format
%%
forward_primitive(User, ?SCCPUDATIND,
		{_Board, _EvntType, _Suid, _Connid, OPC}, SccpAllMsgs) ->
	UD = nms_sccp:'SccpUdataRqst'(SccpAllMsgs),
	CL = nms_sccp:'SccpProtoClass'(UD#'SccpUdataRqst'.protoclass),
	CD = nms_sccp:'SccpAddr'(UD#'SccpUdataRqst'.calledpty),
	CG = nms_sccp:'SccpAddr'(UD#'SccpUdataRqst'.callingpty),
	IM = nms_sccp:'SccpImportance'(UD#'SccpUdataRqst'.importance),
	SD = nms_sccp:'SccpData'(UD#'SccpUdataRqst'.data),
	% if there is no point code in the calling party address we
	% insert the originating point code from the routing label
	if
		(CG#'SccpAddr'.presind == 0) ->
			NewCG = #'SccpAddr'{presind = 1,
					swtype = CD#'SccpAddr'.swtype, % cheating?
					pointcodeind = 1,
					pointcode = OPC};
		(CG#'SccpAddr'.presind == 1)
				and (CG#'SccpAddr'.pointcodeind == 0) ->
			NewCG = CG#'SccpAddr'{pointcodeind = 1, pointcode = OPC};
		(CG#'SccpAddr'.presind == 1)
				and (CG#'SccpAddr'.pointcodeind == 1) ->
			NewCG = CG
	end,
	CallingParty = address(NewCG),
	CalledParty = address(CD),
	case CL#'SccpProtoClass'.classind of
		0 -> SequenceControl = false;
		1 -> SequenceControl = true
	end,
	case CL#'SccpProtoClass'.msghandling of
		0 -> ReturnOption = false;
		8 -> ReturnOption = true
	end,
	case IM#'SccpImportance'.presind of
		0 -> Importance = none;
		1 -> Importance = IM#'SccpImportance'.impvalue
	end,
	PDULen = SD#'SccpData'.datalen,
	case SD#'SccpData'.presind of
		1 -> <<PDU:PDULen/binary, _Rest/binary>> = SD#'SccpData'.data;
		0 -> PDU = <<>>
	end,
	gen_server:cast(User, {'N', 'UNITDATA', indication, {CalledParty,
			CallingParty, {SequenceControl, ReturnOption, Importance}, PDU}});
forward_primitive(User, ?SCCPSTAIND,  {_, EventType, _, _, _}, SccpAllMsgs) ->
	UD = nms_sccp:'SccpUdataRqst'(SccpAllMsgs),
	gen_server:cast(User, {'N', 'NOTICE', indication,
			{UD#'SccpUdataRqst'.calledpty,
			UD#'SccpUdataRqst'.callingpty, UD#'SccpUdataRqst'.eos,
			EventType, UD#'SccpUdataRqst'.importance,
			UD#'SccpUdataRqst'.data}});
forward_primitive(User, ?SCCPCOORDIND, _MoreInfo, SccpAllMsgs) ->
	CO = nms_sccp:'SccpCoordRqst'(SccpAllMsgs),
	gen_server:cast(User, {'N', 'COORD', indication,
			{CO#'SccpCoordRqst'.assn, CO#'SccpCoordRqst'.smi}});
forward_primitive(User, ?SCCPCOORDCFM, _MoreInfo, SccpAllMsgs) ->
	CO = nms_sccp:'SccpCoordRqst'(SccpAllMsgs),
	gen_server:cast(User, {'N', 'COORD', confirm,
			{CO#'SccpCoordRqst'.assn, CO#'SccpCoordRqst'.smi}});
forward_primitive(User, ?SCCPSTATEIND, _MoreInfo, SccpAllMsgs) ->
	CO = nms_sccp:'SccpCoordRqst'(SccpAllMsgs),
	gen_server:cast(User, {'N', 'STATE', indication,
			{CO#'SccpCoordRqst'.assn, CO#'SccpCoordRqst'.status,
			CO#'SccpCoordRqst'.smi}});
forward_primitive(User, ?SCCPPCSTIND, {_, _, _, _, Opc}, SccpAllMsgs) ->
	CO = nms_sccp:'SccpCoordRqst'(SccpAllMsgs),
	gen_server:cast(User, {'N', 'PCSTATE', indication,
			{Opc, CO#'SccpCoordRqst'.status, none, none}});
forward_primitive(User, ?SCCPCONNIND, {_, _, _, Connid, _}, SccpAllMsgs) ->
	CN = nms_sccp:'SccpConnRqst'(SccpAllMsgs),
	gen_server:cast(User, {'N', 'CONNECT', indication,
			{CN#'SccpConnRqst'.calledpty, CN#'SccpConnRqst'.callingpty,
			none, CN#'SccpConnRqst'.eds, none, CN#'SccpConnRqst'.data,
			CN#'SccpConnRqst'.importance, Connid}});
%forward_primitive(User, ?SCCPCONNCFM, {_, _, _, Connid, _}, SccpAllMsgs) ->
%	CN = nms_sccp:'SccpConnRqst'(SccpAllMsgs),
%	gen_server:cast(User, {'N', 'CONNECT', confirm, 
%			{CN#'SccpConnRqst'.calledpty, CN#'SccpConnRqst'.callingpty,
%			none, CN#'SccpConnRqst'.eds, none, CN#'SccpConnRqst'.data,
%			CN#'SccpConnRqst'.importance, Connid}});
forward_primitive(User, ?SCCPDATIND, {_, _, _, Connid, _}, SccpAllMsgs) ->
	DA = nms_sccp:'SccpDataRqst'(SccpAllMsgs),
	gen_server:cast(User, {'N', 'DATA', indication,
			{none, DA#'SccpDataRqst'.data, Connid}});
%forward_primitive(User, ?SCCPEDATIND, {_, _, _, Connid, _}, SccpAllMsgs) ->
%	DA = nms_sccp:'SccpDataRqst'(SccpAllMsgs),
%	gen_server:cast(User, {'N', 'EXPEDITEDDATA', indication, 
%			{none, DA#'SccpDataRqst'.data, Connid}});
forward_primitive(User, ?SCCPRESETIND, {_, _, _, Connid, _}, SccpAllMsgs) ->
	RS = nms_sccp:'SccpResetRqst'(SccpAllMsgs),
	gen_server:cast(User, {'N', 'RESET', indication,
			{RS#'SccpResetRqst'.orig, RS#'SccpResetRqst'.cause, Connid}});
forward_primitive(User, ?SCCPRESETCFM, {_, _, _, Connid, _}, SccpAllMsgs) ->
	RS = nms_sccp:'SccpResetRqst'(SccpAllMsgs),
	gen_server:cast(User, {'N', 'RESET', confirm, 
			{RS#'SccpResetRqst'.orig, RS#'SccpResetRqst'.cause, Connid}});
forward_primitive(User, ?SCCPRELIND, {_, _, _, Connid, _}, SccpAllMsgs) ->
	DC = nms_sccp:'SccpRelease'(SccpAllMsgs),
	gen_server:cast(User, {'N', 'DISCONNECT', indication,
			{DC#'SccpRelease'.orig, DC#'SccpRelease'.cause, 
			DC#'SccpRelease'.data, DC#'SccpRelease'.rsppty, 
			DC#'SccpRelease'.importance, Connid}});
forward_primitive(_User, ?SCCPDACKIND, _MoreInfo, _SccpAllMsgs) ->
	ok;
forward_primitive(_User, ?SCCPCONNAUDCFM, _MoreInfo, _SccpAllMsgs) ->
	ok;
forward_primitive(_User, ?SCCPRUNSTATEIND, {_,?SPRS_STANDALONE,SuID,_,OPC}, 
		_SccpAllMsgs) ->
	<<_, N, C, M>> = <<OPC:32>>,
	error_logger:info_msg("SCCP Point code ~w.~w.~w SuID ~w "
			"run state is standalone~n", [N, M, C, SuID]),
	ok;
forward_primitive(_User, ?SCCPRUNSTATEIND, {_,?SPRS_PRIMARY,SuID,_,OPC}, 
		_SccpAllMsgs) ->
	<<_, N, C, M>> = <<OPC:32>>,
	error_logger:info_msg("SCCP Point code ~w.~w.~w SuID ~w "
			"run state is primary~n", [N, M, C, SuID]),
	ok;
forward_primitive(_User, ?SCCPRUNSTATEIND, {_,?SPRS_BACKUP,SuID,_,OPC}, 
		_SccpAllMsgs) ->
	<<_, N, C, M>> = <<OPC:32>>,
	error_logger:info_msg("SCCP Point code ~w.~w.~w SuID ~w "
			"run state is backup~n", [N, M, C, SuID]),
	ok;
forward_primitive(_User, ?SCCPCONGIND, _MoreInfo, _SccpAllMsgs) ->
	ok.
