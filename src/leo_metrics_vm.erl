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
%% ---------------------------------------------------------------------
%% Leo Statistics - Client VM Metrics
%% @doc
%% @end
%%======================================================================
-module(leo_metrics_vm).
-author('Yosuke Hara').

-behaviour(leo_statistics_behaviour).

-include("include/leo_statistics.hrl").
-include_lib("savanna_commons/include/savanna_commons.hrl").
-include_lib("eunit/include/eunit.hrl").

%% api
-export([start_link/1
        ]).

%% callback
-export([handle_notify/0]).


-define(SCHEMA_NAME, 'vm_stats').
-define(NOTIFIER, 'leo_metrics_vm_notifier').


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
start_link(Window) ->
    case catch mnesia:table_info('sv_schemas', all) of
        {'EXIT', _Cause} ->
            timer:apply_after(500, ?MODULE, start_link, [Window]),
            ok;
        _ ->
            ok = leo_statistics_sup:start_child(?MODULE, Window),

            %% create a schema
            NumOfSamples = 3000,
            ok = savanna_commons:create_schema(
                   ?SCHEMA_NAME, [#sv_column{name = ?STAT_VM_TOTAL_MEM,
                                             type = ?COL_TYPE_H_UNIFORM,
                                             constraint = [{?HISTOGRAM_CONS_SAMPLE, NumOfSamples}]},
                                  #sv_column{name = ?STAT_VM_PROCS_MEM,
                                             type = ?COL_TYPE_H_UNIFORM,
                                             constraint = [{?HISTOGRAM_CONS_SAMPLE, NumOfSamples}]},
                                  #sv_column{name = ?STAT_VM_SYSTEM_MEM,
                                             type = ?COL_TYPE_H_UNIFORM,
                                             constraint = [{?HISTOGRAM_CONS_SAMPLE, NumOfSamples}]},
                                  #sv_column{name = ?STAT_VM_ETS_MEM,
                                             type = ?COL_TYPE_H_UNIFORM,
                                             constraint = [{?HISTOGRAM_CONS_SAMPLE, NumOfSamples}]},
                                  #sv_column{name = ?STAT_VM_PROC_COUNT,
                                             type = ?COL_TYPE_H_UNIFORM,
                                             constraint = [{?HISTOGRAM_CONS_SAMPLE, NumOfSamples}]}
                                 ]),

            %% generate metrics from the schema
            ok = savanna_commons:create_metrics_by_schema(?SCHEMA_NAME, ?METRIC_GRP_VM_1MIN,
                                                          timer:seconds(60),  ?NOTIFIER),
            ok = savanna_commons:create_metrics_by_schema(?SCHEMA_NAME, ?METRIC_GRP_VM_5MIN,
                                                          timer:seconds(300), ?NOTIFIER),
            ok
    end.


%%--------------------------------------------------------------------
%% Callbacks
%%--------------------------------------------------------------------
-spec(handle_notify() ->
             ok).
handle_notify() ->
    TotalMem = erlang:memory(total),
    ProcMem  = erlang:memory(processes),
    SysMem   = erlang:memory(system),
    EtsMem   = erlang:memory(ets),
    Procs    = erlang:system_info(process_count),

    savanna_commons:notify(?METRIC_GRP_VM_1MIN, {?STAT_VM_TOTAL_MEM,  TotalMem}),
    savanna_commons:notify(?METRIC_GRP_VM_1MIN, {?STAT_VM_PROCS_MEM,  ProcMem}),
    savanna_commons:notify(?METRIC_GRP_VM_1MIN, {?STAT_VM_SYSTEM_MEM, SysMem}),
    savanna_commons:notify(?METRIC_GRP_VM_1MIN, {?STAT_VM_ETS_MEM,    EtsMem}),
    savanna_commons:notify(?METRIC_GRP_VM_1MIN, {?STAT_VM_PROC_COUNT, Procs}),

    savanna_commons:notify(?METRIC_GRP_VM_5MIN, {?STAT_VM_TOTAL_MEM,  TotalMem}),
    savanna_commons:notify(?METRIC_GRP_VM_5MIN, {?STAT_VM_PROCS_MEM,  ProcMem}),
    savanna_commons:notify(?METRIC_GRP_VM_5MIN, {?STAT_VM_SYSTEM_MEM, SysMem}),
    savanna_commons:notify(?METRIC_GRP_VM_5MIN, {?STAT_VM_ETS_MEM,    EtsMem}),
    savanna_commons:notify(?METRIC_GRP_VM_5MIN, {?STAT_VM_PROC_COUNT, Procs}),

    snmp_generic:variable_set(?SNMP_NODE_NAME, atom_to_list(erlang:node())),
    ok.


%%--------------------------------------------------------------------
%% Inner Functions
%%--------------------------------------------------------------------
