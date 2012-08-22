%%======================================================================
%%
%% Leo Statistics
%%
%% Copyright (c) 2012 Rakuten, Inc.
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
%% ---------------------------------------------------------------------
%% Leo Statistics - Client VM Metrics
%% @doc
%% @end
%%======================================================================
-module(leo_statistics_metrics_vm).

-author('Yosuke Hara').

-behaviour(leo_statistics_behaviour).

-include("include/leo_statistics.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([init/0, sync/1]).

-define(STAT_VM_TOTAL_MEM_1M,  'vm-total-mem-1m').
-define(STAT_VM_PROCS_MEM_1M,  'vm-procs-mem-1m').
-define(STAT_VM_SYSTEM_MEM_1M, 'vm-system-mem-1m').
-define(STAT_VM_ETS_MEM_1M,    'vm-ets-mem-1m').
-define(STAT_VM_PROC_COUNT_1M, 'vm-proc-count-1m').

-define(STAT_VM_TOTAL_MEM_5M,  'vm-total-mem-5m').
-define(STAT_VM_PROCS_MEM_5M,  'vm-procs-mem-5m').
-define(STAT_VM_SYSTEM_MEM_5M, 'vm-system-mem-5m').
-define(STAT_VM_ETS_MEM_5M,    'vm-ets-mem-5m').
-define(STAT_VM_PROC_COUNT_5M, 'vm-proc-count-5m').

-define(STAT_VM_METRICS_1M,    [?STAT_VM_TOTAL_MEM_1M,
                                ?STAT_VM_PROCS_MEM_1M,
                                ?STAT_VM_SYSTEM_MEM_1M,
                                ?STAT_VM_ETS_MEM_1M,
                                ?STAT_VM_PROC_COUNT_1M]).

-define(STAT_VM_METRICS_5M,    [?STAT_VM_TOTAL_MEM_5M,
                                ?STAT_VM_PROCS_MEM_5M,
                                ?STAT_VM_SYSTEM_MEM_5M,
                                ?STAT_VM_ETS_MEM_5M,
                                ?STAT_VM_PROC_COUNT_5M]).

-type(interval() :: ?STAT_INTERVAL_1M | ?STAT_INTERVAL_5M).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Initialize metrics.
%%
-spec(init() ->
             ok).
init() ->
    lists:foreach(fun(Name) ->
                          ok = leo_statistics_api:new_histogram(
                                 Name, erlang:round((60 * 1000) / ?DEF_STATISTICS_SYNC_INTERVAL))
                  end, ?STAT_VM_METRICS_1M),
    lists:foreach(fun(Name) ->
                          ok = leo_statistics_api:new_histogram(
                                 Name, erlang:round((300 * 1000) / ?DEF_STATISTICS_SYNC_INTERVAL))
                  end, ?STAT_VM_METRICS_5M),
    ok.


%% @doc Synchronize values.
%%
-spec(sync(interval() | integer()) ->
             ok).
sync(?STAT_INTERVAL_1M = Interval) ->
    ok = set_values(Interval, get_values(Interval, arithmetic_mean));

sync(?STAT_INTERVAL_5M = Interval) ->
    ok = set_values(Interval, get_values(Interval, arithmetic_mean));

sync(_Arg) ->
    TotalMem = erlang:memory(total),
    ProcMem  = erlang:memory(processes),
    SysMem   = erlang:memory(system),
    EtsMem   = erlang:memory(ets),
    Procs    = erlang:system_info(process_count),

    ok = leo_statistics_api:notify(?STAT_VM_TOTAL_MEM_1M,  TotalMem),
    ok = leo_statistics_api:notify(?STAT_VM_TOTAL_MEM_5M,  TotalMem),
    ok = leo_statistics_api:notify(?STAT_VM_PROCS_MEM_1M,  ProcMem),
    ok = leo_statistics_api:notify(?STAT_VM_PROCS_MEM_5M,  ProcMem),
    ok = leo_statistics_api:notify(?STAT_VM_SYSTEM_MEM_1M, SysMem),
    ok = leo_statistics_api:notify(?STAT_VM_SYSTEM_MEM_5M, SysMem),
    ok = leo_statistics_api:notify(?STAT_VM_ETS_MEM_1M,    EtsMem),
    ok = leo_statistics_api:notify(?STAT_VM_ETS_MEM_5M,    EtsMem),
    ok = leo_statistics_api:notify(?STAT_VM_PROC_COUNT_1M, Procs),
    ok = leo_statistics_api:notify(?STAT_VM_PROC_COUNT_5M, Procs),
    ok.


%% @doc Retrieve metric-values by property.
%% @private
-spec(get_values(interval(), atom()) ->
             list()).
get_values(Interval, Property) ->
    Metrics = get_metrics_items(Interval),
    lists:map(fun(Name) ->
                      Values = leo_statistics_api:get_histogram(Name),
                      {Name, proplists:get_value(Property, Values)}
              end, Metrics).


%% @doc PUT metric-values into the SNMP.
%% @private
-spec(set_values(interval(), list()) ->
             ok).
set_values(Interval, Values) ->
    Metrics = get_metrics_items(Interval),
    snmp_generic:variable_set(?SNMP_NODE_NAME, atom_to_list(erlang:node())),
    lists:foreach(fun(Name) ->
                          Value = erlang:round(proplists:get_value(Name, Values)),
                          snmp_generic:variable_set(Name, Value)
                  end, Metrics),
    ok.

%% Retrieve metrics-items
%% @private
-spec(get_metrics_items(integer()) ->
             list()).
get_metrics_items(?STAT_INTERVAL_1M) -> ?STAT_VM_METRICS_1M;
get_metrics_items(?STAT_INTERVAL_5M) -> ?STAT_VM_METRICS_5M;
get_metrics_items(_) -> ?STAT_VM_METRICS_1M ++ ?STAT_VM_METRICS_5M.

