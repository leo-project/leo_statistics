%%======================================================================
%%
%% Leo Statistics
%%
%% Copyright (c) 2012-2014 Rakuten, Inc.
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
%%======================================================================
-module(leo_metrics_vm_notifier).
-author('Yosuke Hara').

-behaviour(svc_notify_behaviour).

-include("include/leo_statistics.hrl").
-include_lib("savanna_commons/include/savanna_commons.hrl").
-include_lib("eunit/include/eunit.hrl").

%% callback
-export([notify/2]).


%%--------------------------------------------------------------------
%% Callback
%%--------------------------------------------------------------------
%% @doc
-spec(notify(atom(), {atom(),any()}) ->
             ok | {error, any()}).
notify(?METRIC_GRP_VM_1MIN = MetricGroup, {?STAT_VM_TOTAL_MEM = Key, Stats}) ->
    io:format("metric-group:~w, key:~w, values:~p",
              [MetricGroup, Key, Stats]),
    set_variable(?SNMP_VM_TOTAL_MEM_1M, Stats);
notify(?METRIC_GRP_VM_1MIN = MetricGroup, {?STAT_VM_PROCS_MEM = Key, Stats}) ->
    io:format("metric-group:~w, key:~w, values:~p",
              [MetricGroup, Key, Stats]),
    set_variable(?SNMP_VM_PROCS_MEM_1M, Stats);
notify(?METRIC_GRP_VM_1MIN = MetricGroup, {?STAT_VM_SYSTEM_MEM = Key, Stats}) ->
    io:format("metric-group:~w, key:~w, values:~p",
              [MetricGroup, Key, Stats]),
    set_variable(?SNMP_VM_SYSTEM_MEM_1M, Stats);
notify(?METRIC_GRP_VM_1MIN = MetricGroup, {?STAT_VM_ETS_MEM = Key, Stats}) ->
    io:format("metric-group:~w, key:~w, values:~p",
              [MetricGroup, Key, Stats]),
    set_variable(?SNMP_VM_ETS_MEM_1M, Stats);
notify(?METRIC_GRP_VM_1MIN = MetricGroup, {?STAT_VM_PROC_COUNT = Key, Stats}) ->
    io:format("metric-group:~w, key:~w, values:~p",
              [MetricGroup, Key, Stats]),
    set_variable(?SNMP_VM_PROC_COUNT_1M, Stats);


notify(?METRIC_GRP_VM_5MIN = MetricGroup, {?STAT_VM_TOTAL_MEM = Key, Stats}) ->
    io:format("metric-group:~w, key:~w, values:~p",
              [MetricGroup, Key, Stats]),
    snmp_generic:variable_set(?SNMP_VM_TOTAL_MEM_5M, Stats),
    ok;
notify(?METRIC_GRP_VM_5MIN = MetricGroup, {?STAT_VM_PROCS_MEM = Key, Stats}) ->
    io:format("metric-group:~w, key:~w, values:~p",
              [MetricGroup, Key, Stats]),
    snmp_generic:variable_set(?SNMP_VM_PROCS_MEM_5M, Stats),
    ok;
notify(?METRIC_GRP_VM_5MIN = MetricGroup, {?STAT_VM_SYSTEM_MEM = Key, Stats}) ->
    io:format("metric-group:~w, key:~w, values:~p",
              [MetricGroup, Key, Stats]),
    snmp_generic:variable_set(?SNMP_VM_SYSTEM_MEM_5M, Stats),
    ok;
notify(?METRIC_GRP_VM_5MIN = MetricGroup, {?STAT_VM_ETS_MEM = Key, Stats}) ->
    io:format("metric-group:~w, key:~w, values:~p",
              [MetricGroup, Key, Stats]),
    snmp_generic:variable_set(?SNMP_VM_ETS_MEM_5M, Stats),
    ok;
notify(?METRIC_GRP_VM_5MIN = MetricGroup, {?STAT_VM_PROC_COUNT = Key, Stats}) ->
    io:format("metric-group:~w, key:~w, values:~p",
              [MetricGroup, Key, Stats]),
    snmp_generic:variable_set(?SNMP_VM_PROC_COUNT_5M, Stats),
    ok;

notify(_,_) ->
    ok.


%%--------------------------------------------------------------------
%% Callback
%%--------------------------------------------------------------------
%% @private
set_variable(Item, Stats) ->
    Mean = leo_misc:get_value(?STAT_ELEMENT_MEAN, Stats, 0),
    snmp_generic:variable_set(Item, Mean),
    ok.
