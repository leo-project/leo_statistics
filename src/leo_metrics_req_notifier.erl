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
%% @doc The notifier of request metrics
%% @reference https://github.com/leo-project/leo_statistics/blob/master/src/leo_metrics_req_notifier.erl
%% @end
%%======================================================================
-module(leo_metrics_req_notifier).
-author('Yosuke Hara').

-behaviour(svc_notify_behaviour).

-include("include/leo_statistics.hrl").
-include_lib("savanna_commons/include/savanna_commons.hrl").
-include_lib("eunit/include/eunit.hrl").

%% callback
-export([notify/1]).


%%--------------------------------------------------------------------
%% Callback
%%--------------------------------------------------------------------
%% @doc Notify the message
-spec(notify(Msg) ->
             ok | {error, any()} when Msg::#sv_result{}).
%% 1min + count
notify(#sv_result{metric_group_name = ?METRIC_GRP_REQ_1MIN,
                  col_name = ?STAT_COUNT_GET,
                  result = Count}) ->
    set_variable(?SNMP_COUNT_READS_1M, Count);

notify(#sv_result{metric_group_name = ?METRIC_GRP_REQ_1MIN,
                  col_name = ?STAT_COUNT_PUT,
                  result = Count}) ->
    set_variable(?SNMP_COUNT_WRITES_1M, Count);

notify(#sv_result{metric_group_name = ?METRIC_GRP_REQ_1MIN,
                  col_name = ?STAT_COUNT_DEL,
                  result = Count}) ->
    set_variable(?SNMP_COUNT_DELETES_1M, Count);


%% 1min + size
notify(#sv_result{metric_group_name = ?METRIC_GRP_REQ_1MIN,
                  col_name = ?STAT_SIZE_GET,
                  result = Count}) ->
    set_variable(?SNMP_SIZE_READS_1M, Count);

notify(#sv_result{metric_group_name = ?METRIC_GRP_REQ_1MIN,
                  col_name = ?STAT_SIZE_PUT,
                  result = Count}) ->
    set_variable(?SNMP_SIZE_WRITES_1M, Count);

notify(#sv_result{metric_group_name = ?METRIC_GRP_REQ_1MIN,
                  col_name = ?STAT_SIZE_DEL,
                  result = Count}) ->
    set_variable(?SNMP_SIZE_DELETES_1M, Count);


%% 5min + count
notify(#sv_result{metric_group_name = ?METRIC_GRP_REQ_5MIN,
                  col_name = ?STAT_COUNT_GET,
                  result = Count}) ->
    set_variable(?SNMP_COUNT_READS_5M, Count);

notify(#sv_result{metric_group_name = ?METRIC_GRP_REQ_5MIN,
                  col_name = ?STAT_COUNT_PUT,
                  result = Count}) ->
    set_variable(?SNMP_COUNT_WRITES_5M, Count);

notify(#sv_result{metric_group_name = ?METRIC_GRP_REQ_5MIN,
                  col_name = ?STAT_COUNT_DEL,
                  result = Count}) ->
    set_variable(?SNMP_COUNT_DELETES_5M, Count);

%% 5min + size
notify(#sv_result{metric_group_name = ?METRIC_GRP_REQ_5MIN,
                  col_name = ?STAT_SIZE_GET,
                  result = Count}) ->
    set_variable(?SNMP_SIZE_READS_5M, Count);

notify(#sv_result{metric_group_name = ?METRIC_GRP_REQ_5MIN,
                  col_name = ?STAT_SIZE_PUT,
                  result = Count}) ->
    set_variable(?SNMP_SIZE_WRITES_5M, Count);

notify(#sv_result{metric_group_name = ?METRIC_GRP_REQ_5MIN,
                  col_name = ?STAT_SIZE_DEL,
                  result = Count}) ->
    set_variable(?SNMP_SIZE_DELETES_5M, Count);


notify(_) ->
    ok.


%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------
%% @private
set_variable(Item, Value) ->
    snmp_generic:variable_set(Item, erlang:round(Value)),
    ok.
