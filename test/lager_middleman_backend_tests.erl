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
%%% Module Declaration ========================================================
-module(lager_middleman_backend_tests).

-include_lib("eunit/include/eunit.hrl").

%%%_* Test Cases ==============================================================
can_install_handlers_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   fun(_) ->
       Handlers = gen_event:which_handlers(lager_event),
       [ ?_assert(
            lists:member(
                    console_handler_name(), Handlers))
       , ?_assert(
            lists:member(
              file_handler_name(file1()), Handlers))
       , ?_assert(
            lists:member(
              {lager_middleman_backend, console_handler_name()},
              Handlers))
       , ?_assert(
            lists:member(
              {lager_middleman_backend, file_handler_name(file2())},
              Handlers))
       ]
   end}.

logged_content_is_equivalent_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   fun(_) ->
       lager:log(error, self(), "weird_message"),
       {ok, Content}  = file:read_file(file1()),
       {ok, Content2} = file:read_file(file2()),
       [ ?_assertEqual(Content, Content2),
         ?_assertMatch({match, _},
                       re:run(binary_to_list(Content), ".*weird_message.*")),
         ?_assertMatch({match, _},
                       re:run(binary_to_list(Content2), ".*weird_message.*"))
       ]
   end}.

%%%_* Test Helpers ============================================================
setup() ->
  file:delete(file1()),
  file:delete(file2()),
  ok       = application:start(compiler),
  ok       = application:start(syntax_tools),
  ok       = application:start(goldrush),
  application:load(lager),
  Handlers = [file_handler(file1()),
              wrap(file_handler(file2())),
              console_handler(),
              wrap(console_handler())
             ],
  ok       = application:set_env(lager, handlers, Handlers),
  ok       = application:start(lager),
  Async    = lager_config:get(async),
  lager_config:set(async, false),
  [{async, Async}].

teardown(Config) ->
  Async = proplists:get_value(async, Config),
  lager_config:set(async, Async),
  ok = application:stop(goldrush),
  ok = application:stop(lager),
  ok = application:stop(syntax_tools),
  ok = application:stop(compiler).

%% File Handler
file_handler(Name) ->
  {file_handler_name(Name), file_handler_config(Name)}.

file_handler_name(Name) ->
  {lager_file_backend, Name}.

file_handler_config(Name) ->
  {Name, error, 10485760, "$D0", 5}.

%% Console Handler
console_handler() ->
  {console_handler_name(), console_handler_config()}.

console_handler_name() ->
  lager_console_backend.

console_handler_config() ->
  debug.

%% Helpers
wrap({{Name, Id}, Config}) ->
  {{lager_middleman_backend, {Name, Id}}, {Name, Config}};
wrap({Name, Config}) ->
  {{lager_middleman_backend, Name}, {Name, Config}}.

file1() -> abs_path("test.log").

file2() -> abs_path("test2.log").

abs_path(Filename) ->
  filename:join([tmp_dir(), Filename]).

tmp_dir() -> "/tmp".

%%%_* Emacs ===================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
