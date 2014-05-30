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
-module(lager_middleman_server).

-behaviour(gen_server).

%% API
-export([start_link/1,
         call/2,
         stop/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(s, {backend :: atom(), state :: term()}).

-type s() :: #s{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the server
-spec start_link({atom(), term()}) -> ignore |
                                      {error, term()} |
                                      {ok, pid()}.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% @doc Make a synchronous request
-spec call(pid(), term()) -> term().
call(Server, Request) ->
    gen_server:call(Server, Request).

%% @doc Stop the server
-spec stop(pid()) -> term().
stop(Server) ->
    gen_server:call(Server, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
-spec init({atom(), term()}) -> ignore |
                                {stop, term()} |
                                {ok, s()} |
                                {ok, s(), timeout()}.
init({Backend, Args}) ->
    case Backend:init(Args) of
        ignore ->
            ignore;
        {stop, Reason} ->
            {stop, Reason};
        {ok, State} ->
            {ok, #s{backend = Backend, state = State}};
        {ok, State, Timeout} ->
            {ok, #s{backend = Backend, state = State}, Timeout}
    end.

%% @private
-spec handle_call(term(), {pid(), reference()}, s()) ->
                         {reply, term(), s()}.
handle_call({handle_call, Req}, _From, S) ->
    #s{backend = Backend, state = State} = S,
    {ok, Reply, NewState} = Backend:handle_call(Req, State),
    {reply, Reply, S#s{state = NewState}};
handle_call({handle_event, Req}, _From, S) ->
    #s{backend = Backend, state = State} = S,
    {ok, NewState} = Backend:handle_event(Req, State),
    {reply, ok, S#s{state = NewState}};
handle_call(stop, _From, S) ->
    Reply = ok,
    {reply, Reply, S};
handle_call(_Req, _From, S) ->
    {reply, ok, S}.

%% @private
-spec handle_cast(term(), s()) -> {noreply, s()}.
handle_cast(_Msg, S) ->
    {noreply, S}.

%% @private
-spec handle_info(term(), s()) -> {noreply, s()}.
handle_info(Request, S) ->
    #s{backend = Backend, state = State} = S,
    {ok, NewState} = Backend:handle_info(Request, State),
    {noreply, S#s{state = NewState}}.

%% @private
-spec terminate(term(), s()) -> ok.
terminate(Reason, #s{backend = Backend, state = State}) ->
    ok = Backend:terminate(Reason, State).

%% @private
-spec code_change(term(), s(), term()) -> {ok, s()}.
code_change(_OldVsn, S, _Extra) ->
    {ok, S}.
