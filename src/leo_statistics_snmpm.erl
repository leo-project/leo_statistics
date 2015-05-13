%%======================================================================
%%
%% Leo Statisitcs
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
%% @doc The SNMP manager
%% @reference https://github.com/leo-project/leo_statistics/blob/master/src/leo_statistics_snmpm.erl
%% @end
%%======================================================================
-module(leo_statistics_snmpm).
-author('Yosuke Hara').

-include("leo_statistics.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start/1, walk/4]).

-define(SNMPM_USER, 'leo_statistics_user').

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Start the snmpm
%%
-spec(start(Node) ->
             ok when Node::atom()).
start(Node) ->
    catch snmpm:start(),
    UserId = gen_user_id(Node),
    case snmpm:which_agents(UserId) of
        [] ->
            snmpm:register_user(UserId, ?SNMPM_USER, undefined);
        _  ->
            ok
    end.


%% @doc Retrieve value from SNMPA
%%
-spec(walk(Node, Address, Port, Oid) ->
             {ok, #snmpa_value{}} when Node::atom(),
                                       Address::string(),
                                       Port::pos_integer(),
                                       Oid::[non_neg_integer()]).
walk(Node, Address, Port, Oid) ->
    UserId = gen_user_id(Node),
    Options = [{engine_id, atom_to_list(UserId)},
               {address, Address},
               {port, Port},
               {community, "public"},
               {version, v2}],

    snmpm:register_agent(UserId, atom_to_list(UserId), Options),
    case snmpm:sync_get(UserId, atom_to_list(UserId), [Oid]) of
        {ok,  {_, _, Result}, _Rest} ->
            [{_, Oid, Type, Value, _}] = Result,
            {ok, #snmpa_value{oid   = Oid,
                              type  = Type,
                              value = Value}};
        {error, Cause} ->
            error_logger:error_msg("Cannot send request:~s, cause:~p", [Address, Cause]),
            {error, Cause}
    end.


%% @doc Generate User ID
%% @private
gen_user_id(Node) ->
    list_to_atom(
      lists:append(
        [atom_to_list(?SNMPM_USER), "_", atom_to_list(Node)])).

