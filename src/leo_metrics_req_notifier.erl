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
-module(leo_metrics_req_notifier).
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
%% 1min
notify(?METRIC_GRP_REQ_1MIN = MetricGroup, {?STAT_COUNT_GET = Key, Count}) ->
    io:format("metric-group:~w, key:~w, values:~p",
              [MetricGroup, Key, Count]),
    snmp_generic:variable_set(?SNMP_COUNT_READS_1M, Count),
    ok;
notify(?METRIC_GRP_REQ_1MIN = MetricGroup, {?STAT_COUNT_PUT = Key, Count}) ->
    io:format("metric-group:~w, key:~w, values:~p",
              [MetricGroup, Key, Count]),
    snmp_generic:variable_set(?SNMP_COUNT_WRITES_1M, Count),
    ok;
notify(?METRIC_GRP_REQ_1MIN = MetricGroup, {?STAT_COUNT_DEL = Key, Count}) ->
    io:format("metric-group:~w, key:~w, values:~p",
              [MetricGroup, Key, Count]),
    snmp_generic:variable_set(?SNMP_COUNT_DELETES_1M, Count),
    ok;

notify(?METRIC_GRP_REQ_1MIN = MetricGroup, {?STAT_SIZE_GET = Key, Size}) ->
    io:format("metric-group:~w, key:~w, values:~p",
              [MetricGroup, Key, Size]),
    snmp_generic:variable_set(?SNMP_SIZE_READS_1M, Size),
    ok;
notify(?METRIC_GRP_REQ_1MIN = MetricGroup, {?STAT_SIZE_PUT = Key, Size}) ->
    io:format("metric-group:~w, key:~w, values:~p",
              [MetricGroup, Key, Size]),
    snmp_generic:variable_set(?SNMP_SIZE_READS_1M, Size),
    ok;
notify(?METRIC_GRP_REQ_1MIN = MetricGroup, {?STAT_SIZE_DEL = Key, Size}) ->
    io:format("metric-group:~w, key:~w, values:~p",
              [MetricGroup, Key, Size]),
    snmp_generic:variable_set(?SNMP_SIZE_DELETES_1M, Size),
    ok;


%% 5min
notify(?METRIC_GRP_REQ_5MIN = MetricGroup, {?STAT_COUNT_GET = Key, Count}) ->
    io:format("metric-group:~w, key:~w, values:~p",
              [MetricGroup, Key, Count]),
    snmp_generic:variable_set(?SNMP_COUNT_READS_5M, Count),
    ok;
notify(?METRIC_GRP_REQ_5MIN = MetricGroup, {?STAT_COUNT_PUT = Key, Count}) ->
    io:format("metric-group:~w, key:~w, values:~p",
              [MetricGroup, Key, Count]),
    snmp_generic:variable_set(?SNMP_COUNT_WRITES_5M, Count),
    ok;
notify(?METRIC_GRP_REQ_5MIN = MetricGroup, {?STAT_COUNT_DEL = Key, Count}) ->
    io:format("metric-group:~w, key:~w, values:~p",
              [MetricGroup, Key, Count]),
    snmp_generic:variable_set(?SNMP_COUNT_DELETES_5M, Count),
    ok;

notify(?METRIC_GRP_REQ_5MIN = MetricGroup, {?STAT_SIZE_GET = Key, Size}) ->
    io:format("metric-group:~w, key:~w, values:~p",
              [MetricGroup, Key, Size]),
    snmp_generic:variable_set(?SNMP_SIZE_READS_5M, Size),
    ok;
notify(?METRIC_GRP_REQ_5MIN = MetricGroup, {?STAT_SIZE_PUT = Key, Size}) ->
    io:format("metric-group:~w, key:~w, values:~p",
              [MetricGroup, Key, Size]),
    snmp_generic:variable_set(?SNMP_SIZE_READS_5M, Size),
    ok;
notify(?METRIC_GRP_REQ_5MIN = MetricGroup, {?STAT_SIZE_DEL = Key, Size}) ->
    io:format("metric-group:~w, key:~w, values:~p",
              [MetricGroup, Key, Size]),
    snmp_generic:variable_set(?SNMP_SIZE_DELETES_5M, Size),
    ok;

notify(_,_) ->
    ok.

