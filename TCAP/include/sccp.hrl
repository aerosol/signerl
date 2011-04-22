%%%---------------------------------------------------------------------
%%% @copyright 2004, 2005 Motivity Telecom
%%% @author Vance Shipley <vances@motivity.ca> [http://www.motivity.ca]
%%% @end
%%%
%%% Copyright (c) 2004, 2005, Motivity Telecom
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
%%%  ITU-T recommendation Q.711 Functional Decsription of 
%%%  the Signalling Connection Control Part describes the
%%%  primitives and their parameters used in the N-Service
%%%  interface.  Each primitive has a record defined here
%%%  containing it's parameters.  Modules using this service
%%%  utilize these records to format messages:
%%%
%%%  {'N', 'N-CONNECT', Connect} when is_record(Connect, 'N-CONNECT')
%%%

%% reference: Table 2/Q.711 - Parameters of the primitive N-CONNECT 
-record('N-CONNECT', {calledAddress, callingAddress, respondAddress,
		expeditedData, qos, userData, connectionID, importance}).

%% reference: Table 3/Q.711 - Parameters of the primitive N-DATA
-record('N-DATA', {userData, connectionID, importance}).

%% reference: Table 4/Q.711 - Parameters of the primitive N-EXPEDITED-DATA
-record('N-EXPEDITED-DATA', {userData, connectionID}).

%% reference: Table 5/Q.711 - Parameters of the primitive N-RESET
-record('N-RESET', {originator, reason, connectionID}).

%% reference: Table 6/Q.711 - Parameters of the primitive N-DISCONNECT 
-record('N-DISCONNECT', {originator, respondAddress, reason, userData,
		connectionID, importance}).

%% reference: Table 8/Q.711 - Parameters of the primitive N-INFORM
-record('N-INFORM', {reason, connectionID, qos}).

%% reference: Table 12/Q.711 - Parameters of the primitive N-UNITDATA
-record('N-UNITDATA', {calledAddress, callingAddress, sequenceControl,
		returnOption, importance, userData}).

%% reference: Table 13/Q.711 - Parameters of the primitive N-NOTICE
-record('N-NOTICE', {calledAddress, callingAddress, reason, userData,
		importance}).

%% reference: Table 15/Q.711 - Parameters of the primitive N-COORD
-record('N-COORD', {affectedSubsystem, multiplicity}).

%% reference: Table 16/Q.711 - Parameters of the primitive N-STATE
-record('N-STATE', {affectedSubsystem, userStatus, multiplicity}).

%% reference: Table 17/Q.711 - Parameters of the primitive N-PCSTATE
-record('N-PCSTATE', {affectedSignallingPoint, signallingPointStatus,
		remoteSCCPStatus, restrictedImportanceLevel}).

