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

%% @doc Git Output Printing Functions

-module(git_io).
-export([print_log/2, print_tree/1]).

-ifdef(TEST).
-include("etest/git_io_test.erl").
-endif.

-include("git.hrl").

%% @doc Print branches out to stdout
%print_branches(Git) ->
  %io:fwrite("Branches:~n").

print_tree([Entry|Rest]) ->
    io:fwrite("~-7.6s", [Entry#tree.mode]),
    io:fwrite("~s ", [Entry#tree.sha]),
    io:fwrite("~s~n", [Entry#tree.name]),
    print_tree(Rest);
print_tree([]) ->
    ok.

%% @doc Traverse the reference, printing out all the log information to stdout
print_log(Git, Refs) ->
    Shas = Refs, % TODO - revparse these
    RevList = git:rev_list(Git, Shas),
    print_log_entries(Git, RevList).

print_log_entries(Git, [Sha|Rest]) ->
    {ok, Commit} = git:object(Git, Sha),
    io:fwrite("commit ~s~n", [Commit#commit.sha]),
    io:fwrite("Author: ~s~n", [Commit#commit.author]),
    io:fwrite("~n"),
    io:fwrite("~s~n", [Commit#commit.message]),
    io:fwrite("~n"),
    print_log_entries(Git, Rest);
print_log_entries(_Git, []) ->
    ok.
