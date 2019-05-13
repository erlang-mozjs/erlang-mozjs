%% @author Kevin Smith <ksmith@basho.com>
%% @copyright 2009-2010 Basho Technologies
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

%% @doc This module manages all of the low-level details surrounding the
%% linked-in driver. It is reponsible for loading and unloading the driver
%% as needed. This module is also reponsible for creating and destroying
%% instances of Javascript VMs.

-module(js_driver).

-define(DEFAULT_HEAP_SIZE, 8). %% MB
-define(DEFAULT_THREAD_STACK, 16). %% MB

-export([new/0, new/2, new/3, destroy/1]).
-export([define_js/2, define_js/3, eval_js/2, eval_js/3]).

%% @spec new() -> {ok, reference()} | {error, atom()} | {error, any()}
%% @doc Create a new Javascript VM instance and preload Douglas Crockford's
%% json2 converter (http://www.json.org/js.html). Uses a default heap
%% size of 8MB and a default thread stack size of 8KB.
new() ->
    new(?DEFAULT_THREAD_STACK, ?DEFAULT_HEAP_SIZE).

%% @spec new(ThreadStackSize::int(), HeapSize::int()) -> {ok, reference()} | {error, atom()} | {error, any()}
%% @doc Create a new Javascript VM instance and preload Douglas Crockford's
%% json2 converter (http://www.json.org/js.html)
new(ThreadStackSize, HeapSize) ->
    Initializer = fun(X) -> define_js(X, <<"json2.js">>, json_converter()) end,
    new(ThreadStackSize, HeapSize, Initializer).

%% @type init_fun() = function(reference()) -> true | false
%% @spec new(int(), int(), init_fun() | {ModName::atom(), FunName::atom()}) -> {ok, reference()} | {error, atom()} | {error, any()}
%% @doc Create a new Javascript VM instance. The function arguments control how the VM instance is initialized.
%% User supplied initializers must return true or false.
new(ThreadStackSize, HeapSize, Initializer) when is_function(Initializer) ->
    {ok, Port} = mozjs_nif:sm_init(ThreadStackSize, HeapSize),
    case Initializer(Port) of
        ok ->
            {ok, Port};
        {error, Error} ->
            js_driver:destroy(Port),
            error_logger:error_report(Error),
            throw({error, init_failed})
    end;
new(ThreadStackSize, HeapSize, {InitMod, InitFun}) ->
    Initializer = fun(X) -> InitMod:InitFun(X) end,
    new(ThreadStackSize, HeapSize, Initializer).

%% @spec destroy(reference()) -> ok
%% @doc Destroys a Javascript VM instance
destroy(Ctx) ->
    mozjs_nif:sm_stop(Ctx).

%% @spec define_js(reference(), {file, list()} | binary()) -> ok | {error, any()}
%% @doc Define a Javascript expression:
%% js_driver:define_js(Ctx, &lt;&lt;"var x = 100;"&gt;&gt;).
define_js(Ctx, JsSrc) ->
    exec_js(Ctx, JsSrc, no_jsonify, 0).

%% @spec define_js(reference(), binary(), binary()) -> {ok, binary()} | {error, any()}
%% @doc Define a Javascript expression:
%% js_driver:define_js(Ctx, &lt;&lt;"var blah = new Wubba();"&gt;&gt;).
%% Note: Filename is used only as a label for error reporting.
define_js(Ctx, FileName, Js) ->
    exec_js(Ctx, FileName, Js, no_jsonify, 0).

%% @spec eval_js(reference(), {file, list()} | binary()) -> {ok, any()} | {error, any()}
%% @doc Evaluate a Javascript expression and return the result
%% Note: Filename is used only as a label for error reporting.
eval_js(Ctx, JsSrc) ->
    exec_js(Ctx, JsSrc, jsonify, 1).

%% @spec eval_js(reference(), binary(), binary()) -> {ok, any()} | {error, any()}
%% @doc Evaluate a Javascript expression and return the result
%% Note: Filename is used only as a label for error reporting.
eval_js(Ctx, FileName, Js) ->
    exec_js(Ctx, FileName, Js, jsonify, 1).

%% Internal functions
%% @private
jsonify(Code) when is_binary(Code) ->
    {Body, <<LastChar:8>>} = split_binary(Code, size(Code) - 1),
    C = case LastChar of
            $; ->
                Body;
            _ ->
                Code
        end,
    list_to_binary([<<"JSON.stringify(">>, C, <<");">>]).

%% @private
exec_js(Ctx, {file, FileName}, Jsonify, HandleRetval) ->
    {ok, File} = file:read_file(FileName),
    exec_js(Ctx, list_to_binary(FileName), File, Jsonify, HandleRetval);
exec_js(Ctx, Js, Jsonify, HandleRetval) when is_binary(Js) ->
    exec_js(Ctx, <<"unnamed">>, Js, Jsonify, HandleRetval).

exec_js(Ctx, FileName, Js, jsonify, HandleRetval) ->
    exec_js(Ctx, FileName, jsonify(Js), no_jsonify, HandleRetval);
exec_js(Ctx, FileName, Js, _Jsonify, HandleRetval) when is_binary(FileName), is_binary(Js) ->
    case mozjs_nif:sm_eval(Ctx, FileName, Js, HandleRetval) of
        ok ->
            ok;
	{ok, <<"undefined">>} ->
            {error, mozjs_script_interrupted};
        {ok, Result} ->
            {ok, mochijson2:decode(Result)};
        {error, ErrorJson} when is_binary(ErrorJson) ->
            case mochijson2:decode(ErrorJson) of
                {struct, Error} ->
                    {error, Error};
                _ ->
                    {error, ErrorJson}
            end;
        {error, Error} ->
            {error, Error}
    end.

%% @private
priv_dir() ->
    %% Hacky workaround to handle running from a standard app directory
    %% and .ez package
    case code:priv_dir('erlang-mozjs') of
        {error, bad_name} ->
            filename:join([filename:dirname(code:which(?MODULE)), "..", "priv"]);
        Dir ->
            Dir
    end.

%% @private
json_converter() ->
    is_pid(erlang:whereis(js_cache)) orelse js_cache:start_link(),
    FileName = filename:join([priv_dir(), "json2.js"]),
    case js_cache:fetch(FileName) of
        error ->
            {ok, Contents} = file:read_file(FileName),
            js_cache:store(FileName, Contents),
            Contents;
        Contents ->
            Contents
    end.
