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
%% Leo Statistics - Supervisor.
%% @doc
%% @end
%%======================================================================
-module(leo_statistics_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("eunit/include/eunit.hrl").

start_link() ->
    Res = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    ok = leo_statistics_api:start(?MODULE, 'leo_statistics',
                                   [{'snmp', [leo_statistics_metrics_vm,
                                              leo_statistics_metrics_req]},
                                    {'stat', [leo_statistics_metrics_vm]}]),

    Res.

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    %% ChildProc = {leo_statistics_req_counter,
    %%              {leo_statistics_req_counter, start_link, []},
    %%              permanent, 2000, worker, [leo_statistics_req_counter]},
    {ok, {SupFlags, []}}.

