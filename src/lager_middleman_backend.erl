%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2014 Klarna AB
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(lager_middleman_backend).

-behaviour(gen_event).

-export([init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {middleman :: pid()}).

-type state() :: #state{}.

%% @private
-spec init(term()) -> {ok, state()} | {error, term()}.
init(Args) ->
    case lager_middleman_server:start_link(Args) of
        ignore ->
            ignore;
        {error, Reason} ->
            {error, Reason};
        {ok, Middleman} ->
            {ok, #state{middleman = Middleman}}
    end.

%% @private
-spec handle_call(term(), state()) -> {ok, term(), state()}.
handle_call(Request0, #state{middleman = Middleman} = State) ->
    Request = {handle_call, Request0},
    Reply   = lager_middleman_server:call(Middleman, Request),
    {ok, Reply, State}.

%% @private
-spec handle_event(term(), state()) -> {ok, state()}.
handle_event(Request0, #state{middleman = Middleman} = State) ->
    Request = {handle_event, Request0},
    _Reply  = lager_middleman_server:call(Middleman, Request),
    {ok, State}.

%% @private
-spec handle_info(term(), state()) -> {ok, state()}.
handle_info(_Info, State) ->
    {ok, State}.

%% @private
-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{middleman = Middleman}) ->
    lager_middleman_server:stop(Middleman),
    ok.

%% @private
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
