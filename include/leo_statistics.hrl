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
%% Leo Statistics
%% @doc
%% @end
%%======================================================================
-define(STAT_INTERVAL_1S,   1). %% for test
-define(STAT_INTERVAL_3S,   3). %% for test
-define(STAT_INTERVAL_1M,  60).
-define(STAT_INTERVAL_5M, 300).

-ifdef(TEST).
-define(SNMP_SYNC_INTERVAL_S,     1000). %% 1 sec
-define(SNMP_SYNC_INTERVAL_L,     3000). %% 3 sec
-define(STATISTICS_SYNC_INTERVAL, 1000). %% 1 sec
-else.
-define(SNMP_SYNC_INTERVAL_S,     ?STAT_INTERVAL_1M * 1000). %%  60 sec %% short
-define(SNMP_SYNC_INTERVAL_L,     ?STAT_INTERVAL_5M * 1000). %% 300 sec %% long
-define(STATISTICS_SYNC_INTERVAL, 10000). %%  10 sec
-endif.

-define(SNMP_NODE_NAME, 'node-name').
-define(STAT_REQ_GET,   'req_get').
-define(STAT_REQ_PUT,   'req_put').
-define(STAT_REQ_DEL,   'req_del').


%% @doc SNMPA Value
-record(snmpa_value, {oid = []  :: list(),
                      type = [] :: string(),
                      value     :: any()
                     }).
