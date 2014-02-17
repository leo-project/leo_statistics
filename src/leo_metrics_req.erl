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
-module(leo_metrics_req).
-author('Yosuke Hara').

-behaviour(leo_statistics_behaviour).

-include("include/leo_statistics.hrl").
-include_lib("savanna_commons/include/savanna_commons.hrl").
-include_lib("eunit/include/eunit.hrl").

%% api
-export([start_link/1,
         notify/1, notify/2
        ]).

%% callback
-export([handle_notify/0]).

-define(SCHEMA_NAME, 'access_stats').
-define(NOTIFIER, 'leo_metrics_req_notifier').


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
            _NumOfSamples = 3000,
            ok = savanna_commons:create_schema(
                   ?SCHEMA_NAME, [
                                  %% @TODO - histogram
                                  %% #sv_column{name = ?STAT_HISTO_GET,
                                  %%            type = ?COL_TYPE_H_UNIFORM,
                                  %%            constraint = [{?HISTOGRAM_CONS_SAMPLE, NumOfSamples}]},
                                  %% #sv_column{name = ?STAT_HISTO_PUT,
                                  %%            type = ?COL_TYPE_H_UNIFORM,
                                  %%            constraint = [{?HISTOGRAM_CONS_SAMPLE, NumOfSamples}]},
                                  %% #sv_column{name = ?STAT_HISTO_DEL,
                                  %%            type = ?COL_TYPE_H_UNIFORM,
                                  %%            constraint = [{?HISTOGRAM_CONS_SAMPLE, NumOfSamples}]},

                                  %% counter-1: # of request
                                  #sv_column{name = ?STAT_COUNT_GET,
                                             type = ?COL_TYPE_COUNTER,
                                             constraint = []},
                                  #sv_column{name = ?STAT_COUNT_PUT,
                                             type = ?COL_TYPE_COUNTER,
                                             constraint = []},
                                  #sv_column{name = ?STAT_COUNT_DEL,
                                             type = ?COL_TYPE_COUNTER,
                                             constraint = []},

                                  %% counter-1: summary of file-size
                                  #sv_column{name = ?STAT_SIZE_GET,
                                             type = ?COL_TYPE_COUNTER,
                                             constraint = []},
                                  #sv_column{name = ?STAT_SIZE_PUT,
                                             type = ?COL_TYPE_COUNTER,
                                             constraint = []},
                                  #sv_column{name = ?STAT_SIZE_DEL,
                                             type = ?COL_TYPE_COUNTER,
                                             constraint = []}
                                 ]),

            %% generate metrics from the schema
            ok = savanna_commons:create_metrics_by_schema(?SCHEMA_NAME, ?METRIC_GRP_REQ_1MIN,
                                                          timer:seconds(60),  ?NOTIFIER),
            ok = savanna_commons:create_metrics_by_schema(?SCHEMA_NAME, ?METRIC_GRP_REQ_5MIN,
                                                          timer:seconds(300), ?NOTIFIER),
            ok
    end.


%% @doc Put a value into the savanna from an application
%%
-spec(notify(atom()) ->
             ok | {error, any()}).
notify(Column) ->
    notify(Column, 0).

-spec(notify(atom(), pos_integer()) ->
             ok | {error, any()}).
notify(?STAT_COUNT_GET = Key, Size) ->
    savanna_commons:notify(?METRIC_GRP_REQ_1MIN, {Key, 1}),
    savanna_commons:notify(?METRIC_GRP_REQ_1MIN, {?STAT_SIZE_GET,  Size}),

    savanna_commons:notify(?METRIC_GRP_REQ_5MIN, {Key, 1}),
    savanna_commons:notify(?METRIC_GRP_REQ_5MIN, {?STAT_SIZE_GET,  Size}),

    %% savanna_commons:notify(?METRIC_GRP_REQ_1MIN, {?STAT_HISTO_GET, Size}),
    %% savanna_commons:notify(?METRIC_GRP_REQ_5MIN, {?STAT_HISTO_GET, Size}),
    ok;
notify(?STAT_COUNT_PUT = Key, Size) ->
    savanna_commons:notify(?METRIC_GRP_REQ_1MIN, {Key, 1}),
    savanna_commons:notify(?METRIC_GRP_REQ_1MIN, {?STAT_SIZE_PUT,  Size}),

    savanna_commons:notify(?METRIC_GRP_REQ_5MIN, {Key, 1}),
    savanna_commons:notify(?METRIC_GRP_REQ_5MIN, {?STAT_SIZE_PUT,  Size}),

    %% savanna_commons:notify(?METRIC_GRP_REQ_1MIN, {?STAT_HISTO_PUT, Size}),
    %% savanna_commons:notify(?METRIC_GRP_REQ_5MIN, {?STAT_HISTO_PUT, Size}),
    ok;
notify(?STAT_COUNT_DEL = Key, Size) ->
    savanna_commons:notify(?METRIC_GRP_REQ_1MIN, {Key, 1}),
    savanna_commons:notify(?METRIC_GRP_REQ_1MIN, {?STAT_SIZE_DEL,  Size}),

    savanna_commons:notify(?METRIC_GRP_REQ_5MIN, {Key, 1}),
    savanna_commons:notify(?METRIC_GRP_REQ_5MIN, {?STAT_SIZE_DEL,  Size}),

    %% savanna_commons:notify(?METRIC_GRP_REQ_1MIN, {?STAT_HISTO_DEL, Size}),
    %% savanna_commons:notify(?METRIC_GRP_REQ_5MIN, {?STAT_HISTO_DEL, Size}),
    ok;
notify(_,_) ->
    ok.


%%--------------------------------------------------------------------
%% Callback
%%--------------------------------------------------------------------
handle_notify() ->
    ?debugVal('handle_notify'),
    ok.

%%--------------------------------------------------------------------
%% Internal Function
%%--------------------------------------------------------------------
