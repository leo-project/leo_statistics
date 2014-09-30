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
%% Leo Statistics - API.
%% @doc The statistics API
%% @reference [https://github.com/leo-project/leo_statistics/blob/master/src/leo_statistics_api.erl]
%% @end
%%======================================================================
-module(leo_statistics_api).
-author('Yosuke Hara').

-include_lib("eunit/include/eunit.hrl").

-export([start_link/1,
         create_tables/2
        ]).

-define(env_snmp_agent(Server),
        case application:get_env(Server, snmp_agent) of
            {ok, _SNMPAgent} -> _SNMPAgent;
            _ -> []
        end).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Start the SNMP server
%%
-spec(start_link(Application) ->
             ok | {error, any()} when Application::atom()).
start_link(Application) ->
    case ?env_snmp_agent(Application) of
        [] ->
            ok;
        SNMPAgent ->
            ok = application:start(leo_statistics),
            ok = application:start(snmp),

            case catch snmpa:load_mibs(snmp_master_agent, [SNMPAgent]) of
                ok ->
                    ok;
                Error ->
                    Error
            end
    end.

%% @doc Create the stat's tables
%%
-spec(create_tables(MnesiaDiscType, Nodes) ->
             ok when MnesiaDiscType::disc_copies|ram_copies,
                     Nodes::[atom()]).
create_tables(MnesiaDiscType, Nodes) ->
    _ = mnesia:start(),
    _ = svc_tbl_schema:create_table(MnesiaDiscType, Nodes),
    _ = svc_tbl_column:create_table(MnesiaDiscType, Nodes),
    _ = svc_tbl_metric_group:create_table(MnesiaDiscType, Nodes),
    ok.
