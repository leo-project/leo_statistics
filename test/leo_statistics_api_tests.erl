%%====================================================================
%%
%% Leo Statistics
%%
%% Copyright (c) 2012
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
%% -------------------------------------------------------------------
%% Leo Statistics - EUnit
%% @author yosuke hara
%% @doc
%% @end
%%====================================================================
-module(leo_statistics_api_tests).
-author('yosuke hara').
-vsn('0.9.1').

-include("leo_statistics.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST FUNCTIONS
%%--------------------------------------------------------------------
-ifdef(EUNIT).

statistics_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [{with, [T]} || T <- [fun waiting_for_/1,
                           fun inspect_/1
                          ]]}.

setup() ->
    timer:sleep(2000),
    ok.

teardown(_) ->
    ok.

waiting_for_(_) ->
    timer:sleep(2000),
    ok.

inspect_(_) ->
    timer:sleep(1000),

    %% Retrieve node-name.
    Res0 = os:cmd("snmpwalk -v 2c -c public 127.0.0.1:4160 .1.3.6.1.4.1.35450.17.1"),
    ?debugVal(Res0),
    ?assertEqual("SNMPv2-SMI::enterprises.35450.17.1.0 = STRING: \"statistics@127.0.0.1\"\n", Res0),

    %% Erlang-VM's Values.
    Res1 = os:cmd("snmpwalk -v 2c -c public 127.0.0.1:4160 .1.3.6.1.4.1.35450.17.7"),
    Res2 = os:cmd("snmpwalk -v 2c -c public 127.0.0.1:4160 .1.3.6.1.4.1.35450.17.8"),
    Res3 = os:cmd("snmpwalk -v 2c -c public 127.0.0.1:4160 .1.3.6.1.4.1.35450.17.9"),
    Res4 = os:cmd("snmpwalk -v 2c -c public 127.0.0.1:4160 .1.3.6.1.4.1.35450.17.10"),
    Res5 = os:cmd("snmpwalk -v 2c -c public 127.0.0.1:4160 .1.3.6.1.4.1.35450.17.11"),
    ?debugVal({Res1, Res2, Res3, Res4, Res5}),

    ?assertEqual(true, 0 < list_to_integer(lists:last(string:tokens(Res1, " \n")))),
    ?assertEqual(true, 0 < list_to_integer(lists:last(string:tokens(Res2, " \n")))),
    ?assertEqual(true, 0 < list_to_integer(lists:last(string:tokens(Res3, " \n")))),
    ?assertEqual(true, 0 < list_to_integer(lists:last(string:tokens(Res4, " \n")))),
    ?assertEqual(true, 0 < list_to_integer(lists:last(string:tokens(Res5, " \n")))),
    ok.

-endif.
