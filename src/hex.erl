%% -*- erlang -*-
%%
%% ErlanGit: an implementation of the Git in Erlang
%%  created by Scott Chacon https://github.com/schacon/erlangit
%%
%% Copyright (c) 2011 Basho Technologies, Inc.  All Rights Reserved.
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

%% @doc 
%% via Steve Vinoski
%% http://necrobious.blogspot.com/2008/03/binary-to-hex-string-back-to-binary-in.html

-module(hex).
-author('Steve Vinoski <steve@basho.com>').

-export([bin_to_hexstr/1, list_to_hexstr/1, hexstr_to_bin/1]).

bin_to_hexstr(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b", [X]) ||
                      X <- binary_to_list(Bin)]).

list_to_hexstr(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b", [X]) ||
                      X <- Bin]).

hexstr_to_bin(S) ->
    hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
    list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
    {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
    hexstr_to_bin(T, [V | Acc]).
