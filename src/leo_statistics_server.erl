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
%% ---------------------------------------------------------------------
%% Leo Statistics - Server
%% @doc
%% @end
%%======================================================================
-module(leo_statistics_server).
-author('Yosuke Hara').

-behaviour(gen_server).

-include("include/leo_statistics.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/3, stop/1, sync/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(DEF_TIMEOUT, 30000).

-record(state, {id            :: atom(),
                module        :: atom(),
                interval      :: integer(),
                timestamp = 0 :: integer()}).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
start_link(Id, Module, Interval) ->
    gen_server:start_link({local, Id}, ?MODULE, [Id, Module, Interval], []).

stop(Id) ->
    gen_server:call(Id, stop, ?DEF_TIMEOUT).


%% @doc Synchronize a value.
%%
-spec(sync('snmp_server' | 'stat_server', integer()) ->
             ok).
sync(Id, Interval) ->
    gen_server:cast(Id, {sync, Interval}).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
init([Id, Module, Interval]) ->
    catch erlang:apply(Module, init, []),
    defer_sync(Id, Interval),

    {ok, #state{id       = Id,
                module   = Module,
                interval = Interval}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};


handle_call(_, _From, State) ->
    {reply, ok, State}.


%% Function: handle_cast(Msg, State) -> {noreply, State}          |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
handle_cast({sync, Interval}, #state{module = Module} = State) ->
    NewInterval = erlang:round(Interval / 1000),
    catch erlang:apply(Module, handle_call, [{sync, NewInterval}]),

    NewState = maybe_sync(State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Function: handle_info(Info, State) -> {noreply, State}          |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% INNER FUNCTIONS
%%--------------------------------------------------------------------
maybe_sync(#state{id = Id,
                  interval  = Interval,
                  timestamp = Timestamp} = State) ->
    ThisTime = leo_date:now() * 1000,

    case ((ThisTime - Timestamp) < Interval) of
        true ->
            State;
        false ->
            defer_sync(Id, Interval),
            State#state{timestamp = ThisTime}
    end.


defer_sync(Id, Interval) ->
    timer:apply_after(Interval, ?MODULE, sync, [Id, Interval]).

