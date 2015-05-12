%%======================================================================
%%
%% Leo Statostics
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
%% Leo Statistics
%% @doc
%% @end
%%======================================================================
-module(leo_statistics).

-author('Yosuke Hara').

%% Application and Supervisor callbacks
-export([start/0, stop/0]).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start() ->
    application:start(leo_statistics).

stop() ->
    application:stop(leo_statistics),
    init:stop().

