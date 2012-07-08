%%======================================================================
%%
%% Leo Statisitcs
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
%% Leo Statistics - Server
%% @doc
%% @end
%%======================================================================
-module(leo_statistics_server).
-author('Yosuke Hara').
-vsn('0.9.0').

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


-define(env_snmp_sync_interval_s(Server),
        case application:get_env(Server, snmp_sync_interval_s) of
            {ok, SyncInterval_S} when is_integer(SyncInterval_S) -> SyncInterval_S;
            _ -> ?DEF_SNMP_SYNC_INTERVAL_S
        end).

-define(env_snmp_sync_interval_l(Server),
        case application:get_env(Server, snmp_sync_interval_l) of
            {ok, SyncInterval_L} when is_integer(SyncInterval_L) -> SyncInterval_L;
            _ -> ?DEF_SNMP_SYNC_INTERVAL_L
        end).

-define(env_statistics_sync_interval(Server),
        case application:get_env(Server, statistics_sync_interval) of
            {ok, SyncInterval} when is_integer(SyncInterval) -> SyncInterval;
            _ -> ?DEF_STATISTICS_SYNC_INTERVAL
        end).


-record(state, {id            :: atom(),
                app           :: atom(),
                modules       :: atom(),
                interval      :: integer(),
                timestamp = 0 :: integer()}).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
start_link(Id, AppName, Fun) ->
    gen_server:start_link({local, Id}, ?MODULE, [Id, AppName, Fun], []).

stop(Id) ->
    gen_server:call(Id, stop).


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
init([Id, AppName, Mods]) ->
    lists:foreach(fun(Module) ->
                          catch erlang:apply(Module, init, [])
                  end, Mods),
    Interval = case Id of
                   'snmp_server_s' -> ?env_snmp_sync_interval_s(AppName);
                   'snmp_server_l' -> ?env_snmp_sync_interval_l(AppName);
                   'stat_server'   -> ?env_statistics_sync_interval(AppName)
               end,
    defer_sync(Id, Interval),

    {ok, #state{id       = Id,
                app      = AppName,
                modules  = Mods,
                interval = Interval}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};


handle_call(_, _From, State) ->
    {reply, ok, State}.


%% Function: handle_cast(Msg, State) -> {noreply, State}          |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
handle_cast({sync, Interval}, #state{modules = Mods} = State) ->
    NewInterval = erlang:round(Interval / 1000),

    lists:foreach(fun(Module) ->
                          catch erlang:apply(Module, sync, [NewInterval])
                  end, Mods),

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
    ThisTime = leo_utils:now() * 1000,

    case ((ThisTime - Timestamp) < Interval) of
        true ->
            State;
        false ->
            defer_sync(Id, Interval),
            State#state{timestamp = ThisTime}
    end.


defer_sync(Id, Interval) ->
    timer:apply_after(Interval, ?MODULE, sync, [Id, Interval]).

