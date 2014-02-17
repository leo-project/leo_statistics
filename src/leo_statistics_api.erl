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
%% @doc
%% @end
%%======================================================================
-module(leo_statistics_api).
-author('Yosuke Hara').

-include_lib("eunit/include/eunit.hrl").

-export([start_link/1]).

-define(env_snmp_agent(Server),
        case application:get_env(Server, snmp_agent) of
            {ok, _SNMPAgent} -> _SNMPAgent;
            _ -> []
        end).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Start SNMP server
%%
-spec(start_link(atom()) ->
             ok | {error, any()}).
start_link(Application) ->
    case ?env_snmp_agent(Application) of
        [] ->
            ok;
        SNMPAgent ->
            application:start(leo_statistics),
            application:start(snmp),

            case catch snmpa:load_mibs(snmp_master_agent, [SNMPAgent]) of
                ok ->
                    %% folsom
                    ChildSpec1 = {folsom,
                                  {folsom_sup, start_link, []},
                                  permanent, 2000, supervisor, [folsom]},
                    {ok, _} = supervisor:start_child(leo_statistics_sup, ChildSpec1),
                    ok;
                Error ->
                    Error
            end
    end.
