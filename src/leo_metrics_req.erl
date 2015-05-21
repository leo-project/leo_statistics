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
%% @doc The metrics of request
%% @reference https://github.com/leo-project/leo_statistics/blob/master/src/leo_metrics_req.erl
%% @end
%%======================================================================
-module(leo_metrics_req).
-author('Yosuke Hara').

-behaviour(leo_statistics_behaviour).

-include("include/leo_statistics.hrl").
-include_lib("savanna_commons/include/savanna_commons.hrl").
-include_lib("eunit/include/eunit.hrl").

%% api
-export([start_link/1, start_link/2,
         notify/1, notify/2
        ]).

%% callback
-export([handle_notify/0]).

-define(SCHEMA_NAME, << "access_stats" >>).
-define(NOTIFIER, 'leo_metrics_req_notifier').


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
    try
        ok = savanna_commons:create_schema(
               ?SCHEMA_NAME, [
                              %% counter-1: # of request
                              #?SV_COLUMN{name = ?STAT_COUNT_GET,
                                          type = ?COL_TYPE_COUNTER,
                                          constraint = []},
                              #?SV_COLUMN{name = ?STAT_COUNT_PUT,
                                          type = ?COL_TYPE_COUNTER,
                                          constraint = []},
                              #?SV_COLUMN{name = ?STAT_COUNT_DEL,
                                          type = ?COL_TYPE_COUNTER,
                                          constraint = []}

                              %% @TODO summary of file-size
                              %% #?SV_COLUMN{name = ?STAT_SIZE_GET,
                              %%            type = ?COL_TYPE_COUNTER,
                              %%            constraint = []},
                              %% #?SV_COLUMN{name = ?STAT_SIZE_PUT,
                              %%            type = ?COL_TYPE_COUNTER,
                              %%            constraint = []},
                              %% #?SV_COLUMN{name = ?STAT_SIZE_DEL,
                              %%            type = ?COL_TYPE_COUNTER,
                              %%            constraint = []},
                              %%
                              %% @TODO - histogram
                              %% #?SV_COLUMN{name = ?STAT_HISTO_GET,
                              %%            type = ?COL_TYPE_H_UNIFORM,
                              %%            constraint = [{?HISTOGRAM_CONS_SAMPLE, NumOfSamples}]},
                              %% #?SV_COLUMN{name = ?STAT_HISTO_PUT,
                              %%            type = ?COL_TYPE_H_UNIFORM,
                              %%            constraint = [{?HISTOGRAM_CONS_SAMPLE, NumOfSamples}]},
                              %% #?SV_COLUMN{name = ?STAT_HISTO_DEL,
                              %%            type = ?COL_TYPE_H_UNIFORM,
                              %%            constraint = [{?HISTOGRAM_CONS_SAMPLE, NumOfSamples}]}
                             ]),

        %% generate metrics from the schema
        ok = savanna_commons:create_metrics_by_schema(
               ?SCHEMA_NAME, ?METRIC_GRP_REQ_1MIN, ?SV_WINDOW_1M, ?SV_STEP_1M, ?NOTIFIER),
        ok = savanna_commons:create_metrics_by_schema(
               ?SCHEMA_NAME, ?METRIC_GRP_REQ_5MIN, ?SV_WINDOW_5M, ?SV_STEP_5M, ?NOTIFIER),
        ok
    catch
        _:_ ->
            start_link_1(Window, Times + 1)
    end.


%% @doc Put a value into the savanna from an application
%%
-spec(notify(Column) ->
             ok | {error, any()} when Column::atom()).
notify(Column) ->
    notify(Column, 0).

-spec(notify(Key, Size) ->
             ok | {error, any()} when Key::atom(),
                                      Size::pos_integer()).
notify(?STAT_COUNT_GET = Key,_Size) ->
    savanna_commons:notify(?METRIC_GRP_REQ_1MIN, {Key, 1}),
    savanna_commons:notify(?METRIC_GRP_REQ_5MIN, {Key, 1}),
    ok;
notify(?STAT_COUNT_PUT = Key,_Size) ->
    savanna_commons:notify(?METRIC_GRP_REQ_1MIN, {Key, 1}),
    savanna_commons:notify(?METRIC_GRP_REQ_5MIN, {Key, 1}),
    ok;
notify(?STAT_COUNT_DEL = Key,_Size) ->
    savanna_commons:notify(?METRIC_GRP_REQ_1MIN, {Key, 1}),
    savanna_commons:notify(?METRIC_GRP_REQ_5MIN, {Key, 1}),
    ok;
notify(_,_) ->
    ok.


%%--------------------------------------------------------------------
%% Callback
%%--------------------------------------------------------------------
-spec(handle_notify() ->
             ok).
handle_notify() ->
    snmp_generic:variable_set(?SNMP_NODE_NAME, atom_to_list(erlang:node())),
    ok.
