%%======================================================================
%%
%% Leo Statisitcs
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
-module(leo_statistics_sampler).
-author('Yosuke Hara').

-behaviour(gen_server).

-include("leo_statistics.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/2,
         stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {mod :: atom(),
                timeout = ?DEF_SMPLING_TIMEOUT :: pos_integer()}).

-define(DEF_TIMEOUT, timer:seconds(30)).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
start_link(Mod, ExpireTime) ->
    gen_server:start_link(?MODULE, [Mod, ExpireTime], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
init([Mod, Timeout]) ->
    {ok, #state{mod = Mod,
                timeout = Timeout}, Timeout}.

handle_call(_Request, _From, #state{timeout = Timeout} = State) ->
    Reply = ok,
    {reply, Reply, State, Timeout}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, #state{timeout = Timeout} = State) ->
    {noreply, State, Timeout}.

handle_info(timeout, State=#state{mod = Mod,
                                  timeout = Timeout}) ->
    timer:sleep(erlang:phash2(leo_date:clock(), 250)),
    catch Mod:handle_notify(),
    {noreply, State, Timeout}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
