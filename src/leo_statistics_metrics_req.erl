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
%% Leo Statistics  - Req Client
%% @doc
%% @end
%%======================================================================
-module(leo_statistics_metrics_req).

-author('Yosuke Hara').
-vsn('0.9.0').

-behaviour(leo_statistics_behaviour).

-include("include/leo_statistics.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([init/0, sync/1]).

-define(STAT_HIS_GET,  'his_get').
-define(STAT_HIS_PUT,  'his_put').
-define(STAT_HIS_DEL,  'his_del').

-define(SNMP_REQ_WRITES_1M,  'req-writes-1m').
-define(SNMP_REQ_READS_1M,   'req-reads-1m').
-define(SNMP_REQ_DELETES_1M, 'req-deletes-1m').

-define(SNMP_REQ_WRITES_5M,  'req-writes-5m').
-define(SNMP_REQ_READS_5M,   'req-reads-5m').
-define(SNMP_REQ_DELETES_5M, 'req-deletes-5m').

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Initialize metrics.
%%
-spec(init() ->
             ok).
init() ->
    ok = leo_statistics_api:new_counter(?STAT_REQ_GET),
    ok = leo_statistics_api:new_counter(?STAT_REQ_PUT),
    ok = leo_statistics_api:new_counter(?STAT_REQ_DEL),

    ok = leo_statistics_api:new_history(?STAT_HIS_GET),
    ok = leo_statistics_api:new_history(?STAT_HIS_PUT),
    ok = leo_statistics_api:new_history(?STAT_HIS_DEL),
    ok.


%% @doc Synchronize values.
%%
-spec(sync(?STAT_INTERVAL_1M | ?STAT_INTERVAL_5M) ->
             ok).
sync(?STAT_INTERVAL_1M) ->
    Values = get_values(),

    sync(?STAT_INTERVAL_1M, Values), %% for 1min
    leo_statistics_api:notify(?STAT_HIS_PUT,  proplists:get_value(?STAT_REQ_PUT,  Values)),
    leo_statistics_api:notify(?STAT_HIS_GET,  proplists:get_value(?STAT_REQ_GET,  Values)),
    leo_statistics_api:notify(?STAT_HIS_DEL,  proplists:get_value(?STAT_REQ_DEL,  Values)),

    leo_statistics_api:dec_counter(?STAT_REQ_PUT,  proplists:get_value(?STAT_REQ_PUT,  Values)),
    leo_statistics_api:dec_counter(?STAT_REQ_GET,  proplists:get_value(?STAT_REQ_GET,  Values)),
    leo_statistics_api:dec_counter(?STAT_REQ_DEL,  proplists:get_value(?STAT_REQ_DEL,  Values)),

    Len = round(?STAT_INTERVAL_5M / ?STAT_INTERVAL_1M),
    case leo_statistics_api:get_history(?STAT_HIS_PUT, Len) of
        Histories when length(Histories) >= Len ->
            sync(?STAT_INTERVAL_5M, [{?STAT_REQ_PUT,  leo_statistics_api:sum(?STAT_HIS_PUT,  Len)},
                                     {?STAT_REQ_GET,  leo_statistics_api:sum(?STAT_HIS_GET,  Len)},
                                     {?STAT_REQ_DEL,  leo_statistics_api:sum(?STAT_HIS_DEL,  Len)}
                                    ]),
            leo_statistics_api:clear_history(?STAT_HIS_PUT),
            leo_statistics_api:clear_history(?STAT_HIS_GET),
            leo_statistics_api:clear_history(?STAT_HIS_DEL);
        _ ->
            void
    end,
    ok;

sync(?STAT_INTERVAL_5M) ->
    ok.


sync(?STAT_INTERVAL_1M, Values) ->
    snmp_generic:variable_set(?SNMP_NODE_NAME,      atom_to_list(node())),
    snmp_generic:variable_set(?SNMP_REQ_WRITES_1M,  proplists:get_value(?STAT_REQ_PUT,  Values)),
    snmp_generic:variable_set(?SNMP_REQ_READS_1M,   proplists:get_value(?STAT_REQ_GET,  Values)),
    snmp_generic:variable_set(?SNMP_REQ_DELETES_1M, proplists:get_value(?STAT_REQ_DEL,  Values)),
    ok;

sync(?STAT_INTERVAL_5M, Values) ->
    snmp_generic:variable_set(?SNMP_NODE_NAME,      atom_to_list(node())),
    snmp_generic:variable_set(?SNMP_REQ_WRITES_5M,  proplists:get_value(?STAT_REQ_PUT,  Values)),
    snmp_generic:variable_set(?SNMP_REQ_READS_5M,   proplists:get_value(?STAT_REQ_GET,  Values)),
    snmp_generic:variable_set(?SNMP_REQ_DELETES_5M, proplists:get_value(?STAT_REQ_DEL,  Values)),
    ok.


%% @doc Retrieve metric-values.
%% @private
-spec(get_values() ->
             list()).
get_values() ->
    [{?STAT_REQ_PUT,  leo_statistics_api:get(?STAT_REQ_PUT)},
     {?STAT_REQ_GET,  leo_statistics_api:get(?STAT_REQ_GET)},
     {?STAT_REQ_DEL,  leo_statistics_api:get(?STAT_REQ_DEL)}].

