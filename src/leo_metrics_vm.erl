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
%% @doc The metrics of Erlang VM's statistics
%% @reference https://github.com/leo-project/leo_statistics/blob/master/src/leo_metrics_vm.erl
%% @end
%%======================================================================
-module(leo_metrics_vm).
-author('Yosuke Hara').

-behaviour(leo_statistics_behaviour).

-include("include/leo_statistics.hrl").
-include_lib("savanna_commons/include/savanna_commons.hrl").
-include_lib("eunit/include/eunit.hrl").

%% api
-export([start_link/1, start_link/2]).

%% callback
-export([handle_notify/0]).


-define(SCHEMA_NAME, << "vm_stats" >>).
-define(NOTIFIER, 'leo_metrics_vm_notifier').


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Start the metrics
%%
-spec(start_link(Window) ->
             ok | {error, any()} when Window::non_neg_integer()).
start_link(Window) ->
    start_link(Window, false).

-spec(start_link(Window, IsOnlyStartChild) ->
             ok | {error, any()} when Window::non_neg_integer(),
                                      IsOnlyStartChild::boolean()).
start_link(Window, IsOnlyStartChild) ->
    case IsOnlyStartChild of
        true ->
            leo_statistics_sup:start_child(?MODULE, Window);
        false ->
            case catch mnesia:table_info('sv_schemas', all) of
                {'EXIT', Cause} ->
                    error_logger:error_msg("~p,~p,~p,~p~n",
                                           [{module, ?MODULE_STRING},
                                            {function, "start_link/1"},
                                            {line, ?LINE}, {body, Cause}]),
                    {error, Cause};
                _Ret ->
                    ok = leo_statistics_sup:start_child(?MODULE, Window),
                    start_link_1(Window, 0)
            end
    end.


%% @private
start_link_1(_, ?MAX_RETRY_TIMES) ->
    {error, "Could not create the schemas"};
start_link_1(Window, Times) ->
    timer:sleep(timer:seconds(1)),
    NumOfSamples = 3000,
    try
        ok = savanna_commons:create_schema(
               ?SCHEMA_NAME, [#?SV_COLUMN{name = ?STAT_VM_TOTAL_MEM,
                                          type = ?COL_TYPE_H_UNIFORM,
                                          constraint = [{?HISTOGRAM_CONS_SAMPLE, NumOfSamples}]},
                              #?SV_COLUMN{name = ?STAT_VM_PROCS_MEM,
                                          type = ?COL_TYPE_H_UNIFORM,
                                          constraint = [{?HISTOGRAM_CONS_SAMPLE, NumOfSamples}]},
                              #?SV_COLUMN{name = ?STAT_VM_SYSTEM_MEM,
                                          type = ?COL_TYPE_H_UNIFORM,
                                          constraint = [{?HISTOGRAM_CONS_SAMPLE, NumOfSamples}]},
                              #?SV_COLUMN{name = ?STAT_VM_ETS_MEM,
                                          type = ?COL_TYPE_H_UNIFORM,
                                          constraint = [{?HISTOGRAM_CONS_SAMPLE, NumOfSamples}]},
                              #?SV_COLUMN{name = ?STAT_VM_PROC_COUNT,
                                          type = ?COL_TYPE_H_UNIFORM,
                                          constraint = [{?HISTOGRAM_CONS_SAMPLE, NumOfSamples}]},
                              #?SV_COLUMN{name = ?STAT_VM_USED_PER_ALLOC_MEM,
                                          type = ?COL_TYPE_H_UNIFORM,
                                          constraint = [{?HISTOGRAM_CONS_SAMPLE, NumOfSamples}]},
                              #?SV_COLUMN{name = ?STAT_VM_ALLOC_MEM,
                                          type = ?COL_TYPE_H_UNIFORM,
                                          constraint = [{?HISTOGRAM_CONS_SAMPLE, NumOfSamples}]}
                             ]),

        %% generate metrics from the schema
        ok = savanna_commons:create_metrics_by_schema(
               ?SCHEMA_NAME, ?METRIC_GRP_VM_1MIN, ?SV_WINDOW_1M, ?SV_STEP_1M, ?NOTIFIER),
        ok = savanna_commons:create_metrics_by_schema(
               ?SCHEMA_NAME, ?METRIC_GRP_VM_5MIN, ?SV_WINDOW_5M, ?SV_STEP_5M, ?NOTIFIER),
        ok
    catch
        _:Cause ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                   [{module, ?MODULE_STRING},
                                    {function, "start_link_1/2"},
                                    {line, ?LINE}, {body, Cause}]),
            start_link_1(Window, Times + 1)
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
    UsedPerAllocMem = round(recon_alloc:memory(usage) * 100),
    AllocatedMem    = recon_alloc:memory(allocated),

    savanna_commons:notify(?METRIC_GRP_VM_1MIN, {?STAT_VM_TOTAL_MEM,  TotalMem}),
    savanna_commons:notify(?METRIC_GRP_VM_1MIN, {?STAT_VM_PROCS_MEM,  ProcMem}),
    savanna_commons:notify(?METRIC_GRP_VM_1MIN, {?STAT_VM_SYSTEM_MEM, SysMem}),
    savanna_commons:notify(?METRIC_GRP_VM_1MIN, {?STAT_VM_ETS_MEM,    EtsMem}),
    savanna_commons:notify(?METRIC_GRP_VM_1MIN, {?STAT_VM_PROC_COUNT, Procs}),
    savanna_commons:notify(?METRIC_GRP_VM_1MIN, {?STAT_VM_USED_PER_ALLOC_MEM, UsedPerAllocMem}),
    savanna_commons:notify(?METRIC_GRP_VM_1MIN, {?STAT_VM_ALLOC_MEM,  AllocatedMem}),

    savanna_commons:notify(?METRIC_GRP_VM_5MIN, {?STAT_VM_TOTAL_MEM,  TotalMem}),
    savanna_commons:notify(?METRIC_GRP_VM_5MIN, {?STAT_VM_PROCS_MEM,  ProcMem}),
    savanna_commons:notify(?METRIC_GRP_VM_5MIN, {?STAT_VM_SYSTEM_MEM, SysMem}),
    savanna_commons:notify(?METRIC_GRP_VM_5MIN, {?STAT_VM_ETS_MEM,    EtsMem}),
    savanna_commons:notify(?METRIC_GRP_VM_5MIN, {?STAT_VM_PROC_COUNT, Procs}),
    savanna_commons:notify(?METRIC_GRP_VM_5MIN, {?STAT_VM_USED_PER_ALLOC_MEM, UsedPerAllocMem}),
    savanna_commons:notify(?METRIC_GRP_VM_5MIN, {?STAT_VM_ALLOC_MEM,  AllocatedMem}),

    snmp_generic:variable_set(?SNMP_NODE_NAME, atom_to_list(erlang:node())),
    ok.


%%--------------------------------------------------------------------
%% Inner Functions
%%--------------------------------------------------------------------
