%%======================================================================
%%
%% Leo Statistics
%%
%% Copyright (c) 2012-2015 Rakuten, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% @doc The SNMP manager's operation
%% @reference https://github.com/leo-project/leo_statistics/blob/master/src/leo_statistics_user.erl
%% @end
%%======================================================================
-module(leo_statistics_user).
-author('Yosuke Hara').

-behaviour(snmpm_user).

-export([handle_error/3,
         handle_agent/5,
         handle_pdu/4,
         handle_trap/3,
         handle_inform/3,
         handle_report/3]).

handle_error(ReqId, Reason, UserData) ->
    error_logger:error_msg("Handle error - reqid:~p, reason:~p, user-data:~p",
                           [ReqId, Reason, UserData]),
    ignore.

handle_agent(Addr, Port, Type, SnmpInfo, UserData) ->
    error_logger:error_msg("Handle error - addr:~p, port:~p, type:~p, snmp-info:~p, user-data:~p",
                           [Addr, Port, Type, SnmpInfo, UserData]),
    ignore.

handle_pdu(TargetName, ReqId, SnmpPduInfo, UserData) ->
    error_logger:error_msg("Handle error - target-name:~p, reqid:~p, snmp-pdu-info:~p, user-data:~p",
                           [TargetName, ReqId, SnmpPduInfo, UserData]),
    ignore.

handle_trap(TargetName, SnmpTrapInfo, UserData) ->
    error_logger:error_msg("Handle error - target-name:~p, snmp-trap-info:~p, user-data:~p",
                           [TargetName, SnmpTrapInfo, UserData]),
    ignore.

handle_inform(TargetName, SnmpInformInfo, UserData) ->
    error_logger:error_msg("Handle error - target-name:~p, snmp-inform-info:~p, user-data:~p",
                           [TargetName, SnmpInformInfo, UserData]),
    ignore.

handle_report(TargetName, SnmpReportInfo, UserData) ->
    error_logger:error_msg("Handle error - target-name:~p, snmp-report-info:~p, user-data:~p",
                           [TargetName, SnmpReportInfo, UserData]),
    ignore.

