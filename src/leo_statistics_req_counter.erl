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
%% Leo Statistics - Request Counter
%% @doc
%% @end
%%======================================================================
-module(leo_statistics_req_counter).
-author('Yosuke Hara').
-vsn('0.9.0').

-behaviour(gen_server).

-include("include/leo_statistics.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0, stop/1,
         increment/1, decrement/1, clear/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop(Id) ->
    gen_server:call(Id, stop).


%% @doc Increment a value
%%
-spec(increment(atom()) ->
             ok).
increment(Table) ->
    gen_server:cast(?MODULE, {increment, Table}).


%% @doc Decrement a value
%%
-spec(decrement(atom()) ->
             ok).
decrement(Table) ->
    gen_server:cast(?MODULE, {decrement, Table}).


%% @doc Clear a value
%%
-spec(clear(atom()) ->
             ok).
clear(Table) ->
    gen_server:cast(?MODULE, {clear, Table}).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
init([]) ->
    {ok, null}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_, _From, State) ->
    {reply, ok, State}.


%% Function: handle_cast(Msg, State) -> {noreply, State}          |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
handle_cast({increment, Table}, State) ->
    catch leo_statistics_api:inc_counter(Table, 1),
    {noreply, State};

handle_cast({decrement, Table}, State) ->
    catch leo_statistics_api:dec_counter(Table, 1),
    {noreply, State};

handle_cast({clear, Table}, State) ->
    catch leo_statistics_api:clear_counter(Table),
    {noreply, State};

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

