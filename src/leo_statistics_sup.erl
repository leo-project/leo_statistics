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
%% ---------------------------------------------------------------------
%% Leo Statistics - Supervisor.
%% @doc
%% @end
%%======================================================================
-module(leo_statistics_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/2
        ]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("eunit/include/eunit.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% @doc Launch metrics
%%
-spec(start_child(atom(), pos_integer()) ->
             ok | {error, any()}).
start_child(Module, Window) ->
    PropListOfCounts = supervisor:count_children(?MODULE),
    Specs = leo_misc:get_value('specs', PropListOfCounts),

    Id = list_to_atom(lists:append(["leo_statistics_", integer_to_list(Specs)])),
    ChildSpec = {Id,
                 {leo_statistics_sampler, start_link, [Module, Window]},
                 permanent, 2000, worker, [leo_statistics_sampler]},
    {ok, _Pid} = supervisor:start_child(leo_statistics_sup, ChildSpec),
    ok.


%% ---------------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------------
%% @spec (Params) -> ok
%% @doc stop process.
%% @end
%% @private
init([]) ->
    Children = [
                {savanna_commons_sup,
                 {savanna_commons_sup, start_link, []},
                 permanent,
                 2000,
                 supervisor,
                 [savanna_commons_sup]}
               ],
    {ok, {{one_for_one, 5, 60}, Children}}.
