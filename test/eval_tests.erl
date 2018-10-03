-module(eval_tests).

-include_lib("eunit/include/eunit.hrl").

var_test_() ->
      [fun() ->
               {ok, Handle} = mozjs_nif:sm_init(8, 8),
               ?assertMatch(ok, js:define(Handle, <<"var x = 100;">>)),
               ?assertMatch({ok, 100}, js:eval(Handle, <<"x;">>)),
               mozjs_nif:sm_stop(Handle) end].

null_define_test_() ->
      [fun() ->
               {ok, Handle} = mozjs_nif:sm_init(8, 8),
               ?assertMatch(ok, js:define(Handle, "")),
               mozjs_nif:sm_stop(Handle) end].


function_test_() ->
      [fun() ->
               {ok, Handle} = mozjs_nif:sm_init(8, 8),
               ?assertMatch(ok, js:define(Handle, <<"function add_two(x, y) { return x + y; };">>)),
               ?assertMatch({ok, 95}, js:call(Handle, <<"add_two">>, [85, 10])),
               ?assertMatch({ok, <<"testing123">>}, js:call(Handle, <<"add_two">>, [<<"testing">>, <<"123">>])),
               mozjs_nif:sm_stop(Handle) end,
       fun() ->
               {ok, Handle} = mozjs_nif:sm_init(8, 8),
               ?assertMatch(ok, js:define(Handle, <<"var f = function(x, y) \n{ return y - x; \n};">>)),
               ?assertMatch({ok, 75}, js:call(Handle, <<"f">>, [10, 85])),
               mozjs_nif:sm_stop(Handle) end,
       fun() ->
               {ok, Handle} = mozjs_nif:sm_init(8, 8),
               ?assertMatch(ok, js:define(Handle, <<"function get_first(data) { return data[\"first\"]; };">>)),
               Data = {struct, [{<<"first">>, <<"abc">>}]},
               ?assertMatch({ok, <<"abc">>}, js:call(Handle, <<"get_first">>, [Data])),
               mozjs_nif:sm_stop(Handle) end,
      fun() ->
              %% Regression test case for embedded error properties in function return values
              {ok, Handle} = mozjs_nif:sm_init(8, 8),
              ?assertMatch(ok, js:define(Handle, <<"function return_error_property() { return [{\"value\": \"some_value\", \"list\": [{\"error\": \"some_error\"}]}]; }">>)),
              ?assertMatch({ok,[{struct,[{<<"value">>,<<"some_value">>},{<<"list">>,[{struct,[{<<"error">>,<<"some_error">>}]}]}]}]}, js:call(Handle, <<"return_error_property">>, [])),
              mozjs_nif:sm_stop(Handle) end,
      fun() ->
              %% Regression test case for github issue 42 - quotes in anonymous functions
              {ok, Handle} = mozjs_nif:sm_init(8, 8),
              ?assertMatch({ok, [<<"foo">>, <<"bar">>]}, js:call(Handle, <<"function(x) { return x.split(\" \"); }">>, [<<"foo bar">>])),
              mozjs_nif:sm_stop(Handle) end
      ].

binding_test_() ->
      [fun() ->
               {ok, Handle} = mozjs_nif:sm_init(8, 8),
               ?assertMatch(ok, js:define(Handle, <<"var c = 100;function constant_mult(x) { return x * c; }">>)),
               ?assertMatch({ok, 200}, js:call(Handle, <<"constant_mult">>, [2])),
               ?assertMatch({ok, 1000}, js:call(Handle, <<"constant_mult">>, [2], [{<<"c">>, 500}])),
               mozjs_nif:sm_stop(Handle) end,
       fun() ->
               {ok, Handle} = mozjs_nif:sm_init(8, 8),
               ?assertMatch(ok, js:define(Handle, <<"function constant_div(x) { return x / q; }">>, [{<<"q">>, 2}])),
               ?assertMatch({ok, 5}, js:call(Handle, <<"constant_div">>, [10])),
               ?assertMatch({ok, 3}, js:call(Handle, <<"constant_div">>, [9], [{<<"q">>, 3}])),
               mozjs_nif:sm_stop(Handle) end].

charset_test_() ->
      [fun() ->
           {ok, Handle} = mozjs_nif:sm_init(8, 8),
           %% Kanji character
           Kanji = <<123,34,116,101,120,116,34,58,34,228,188,141,34,125,10>>,
           ?assertMatch(ok, js:define(Handle, <<"function foo(x) { return x; }">>)),
           ?assertMatch({ok, Kanji}, js:call(Handle, <<"foo">>, [Kanji])),
           mozjs_nif:sm_stop(Handle) end].

json_test_() ->
  [fun() ->
       Struct = {struct, [{<<"test">>, <<"1">>}]},
       ?assertMatch(Struct, mochijson2:decode(mochijson2:encode(Struct))) end].

ejslog_test_() ->
      [fun() ->
               {ok, Handle} = mozjs_nif:sm_init(8, 8),
               [] = os:cmd("rm -f /tmp/eval_tests.log"),
               ?assertEqual({ok, true},
                            js_driver:eval_js(Handle, <<"ejsLog('/tmp/eval_tests.log', 'Hello')">>)),
               ?assert(filelib:is_file("/tmp/eval_tests.log")),
               mozjs_nif:sm_stop(Handle)
       end].


error_test_() ->
      [fun() ->
               {ok, Handle} = mozjs_nif:sm_init(8, 8),
               {error, ErrorDesc} = js:define(Handle, <<"functoin foo(x, y) { return true; };">>),
               ?assert(verify_error(ErrorDesc)),
               mozjs_nif:sm_stop(Handle) end,
       fun() ->
               {ok, Handle} = mozjs_nif:sm_init(8, 8),
               ?assertMatch(ok, js:define(Handle, <<"function foo(x, y) { return true; };">>)),
               ?assertEqual({ok, true}, js:eval(Handle, <<"foo(100, 200,);">>)),
               mozjs_nif:sm_stop(Handle) end,
       fun() ->
               {ok, Handle} = mozjs_nif:sm_init(8, 8),
               {error, ErrorDesc} = js:define(Handle, <<"functoin foo() { return \"oops\"; };">>),
               ?assert(verify_error(ErrorDesc)),
               mozjs_nif:sm_stop(Handle) end,
       fun() ->
               {ok, Handle} = mozjs_nif:sm_init(8, 8),
               js:define(Handle, <<"function foo() { throw new Error(\"notfound\"); };">>),
               {error, ErrorDesc} = js:call(Handle, <<"foo">>, []),
               ?assert(verify_error(ErrorDesc)),
               mozjs_nif:sm_stop(Handle) end,
       fun() ->
               {ok, Handle} = mozjs_nif:sm_init(8, 8),
               {error, ErrorDesc} = js:eval(Handle, <<"blah(\"wubba\");">>),
               ?assert(verify_error(ErrorDesc)),
               mozjs_nif:sm_stop(Handle) end].


%% Internal functions
verify_error([{<<"lineno">>, LineNo},
              {<<"message">>, Msg},
              {<<"source">>, Source}]) when is_number(LineNo),
                                                          is_binary(Msg),
                                                          is_binary(Source) ->
    true;
verify_error(Error) ->
    % Shoudn't normally happen.
    ?debugFmt("Error: ~p~n", [Error]),
    false.
