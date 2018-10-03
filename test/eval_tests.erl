-module(eval_tests).

-include_lib("eunit/include/eunit.hrl").

basic_js_test_() ->
	{setup,
		fun() -> {ok, Handle} = mozjs_nif:sm_init(8, 8), Handle end,
		fun(Handle) -> mozjs_nif:sm_stop(Handle) end,
		fun(Handle) -> [
			% Basic variable testing
			{"define variable", ?_assertEqual(ok, js:define(Handle, <<"var x = 100;">>))},
			{"evaluate variable", ?_assertEqual({ok, 100}, js:eval(Handle, <<"x;">>))},
			{"define empty data", ?_assertEqual(ok, js:define(Handle, ""))},

			% Basic function testing
			{"define fun add_two", ?_assertEqual(ok, js:define(Handle, <<"function add_two(x, y) { return x + y; };">>))},
			{"use fun add_two for numbers", ?_assertEqual({ok, 95}, js:call(Handle, <<"add_two">>, [85, 10]))},
			{"use fun add_two for strings", ?_assertEqual({ok, <<"testing123">>}, js:call(Handle, <<"add_two">>, [<<"testing">>, <<"123">>]))},

			{"define subtract fun as var", ?_assertEqual(ok, js:define(Handle, <<"var f = function(x, y) \n{ return y - x; \n};">>))},
			{"use subtract fun", ?_assertEqual({ok, 75}, js:call(Handle, <<"f">>, [10, 85]))},

			{"define JSON extraction fun", ?_assertEqual(ok, js:define(Handle, <<"function get_first(data) { return data[\"first\"]; };">>))},
			{"use JSON exctaction fun", ?_assertEqual({ok, <<"abc">>}, js:call(Handle, <<"get_first">>, [{struct, [{<<"first">>, <<"abc">>}]}]))},

			%% Regression test case for embedded error properties in function return values
			{"define JSON returning fun", ?_assertEqual(ok, js:define(Handle, <<"function return_error_property() { return [{\"value\": \"some_value\", \"list\": [{\"error\": \"some_error\"}]}]; }">>))},
			{"use JSON returning fun", ?_assertEqual({ok,[{struct,[{<<"value">>,<<"some_value">>},{<<"list">>,[{struct,[{<<"error">>,<<"some_error">>}]}]}]}]}, js:call(Handle, <<"return_error_property">>, []))},

			%% Regression test case for github issue basho/erlang_js#42 - quotes in anonymous functions
			{"define anonymous fun with quotes", ?_assertEqual({ok, [<<"foo">>, <<"bar">>]}, js:call(Handle, <<"function(x) { return x.split(\" \"); }">>, [<<"foo bar">>]))}
		] end
      }.

binding_test_() ->
	{setup,
		fun() -> {ok, Handle} = mozjs_nif:sm_init(8, 8), Handle end,
		fun(Handle) -> mozjs_nif:sm_stop(Handle) end,
		fun(Handle) -> [
			{"define fun1 with bindings in place", ?_assertEqual(ok, js:define(Handle, <<"var c = 100;function constant_mult(x) { return x * c; }">>))},
			{"eval fun1 using default val", ?_assertEqual({ok, 200}, js:call(Handle, <<"constant_mult">>, [2]))},
			{"eval fun1 using other val", ?_assertEqual({ok, 1000}, js:call(Handle, <<"constant_mult">>, [2], [{<<"c">>, 500}]))},
			{"define fun2 with bindings", ?_assertEqual(ok, js:define(Handle, <<"function constant_div(x) { return x / q; }">>, [{<<"q">>, 2}]))},
			{"eval fun2 using default val", ?_assertEqual({ok, 5}, js:call(Handle, <<"constant_div">>, [10]))},
			{"eval fun2 using other val", ?_assertEqual({ok, 3}, js:call(Handle, <<"constant_div">>, [9], [{<<"q">>, 3}]))}
		] end
      }.

charset_test_() ->
	%% Kanji character
	Kanji = <<123,34,116,101,120,116,34,58,34,228,188,141,34,125,10>>,
	{setup,
		fun() -> {ok, Handle} = mozjs_nif:sm_init(8, 8), Handle end,
		fun(Handle) -> mozjs_nif:sm_stop(Handle) end,
		fun(Handle) -> [
			{"Define fun", ?_assertEqual(ok, js:define(Handle, <<"function foo(x) { return x; }">>))},
			{"Check if this fun can return Kanji?", ?_assertEqual({ok, Kanji}, js:call(Handle, <<"foo">>, [Kanji]))}
		] end
      }.

json_test_() ->
	Struct1 = {struct, [{<<"test">>, <<"1">>}]},
	Struct2 = {struct, [{<<"test2">>, 2}]},
	[
		{"Check if we can parse back and forth strings", ?_assertEqual(Struct1, mochijson2:decode(mochijson2:encode(Struct1)))},
		{"Check if we can parse back and forth numbers", ?_assertEqual(Struct2, mochijson2:decode(mochijson2:encode(Struct2)))}
	].

ejslog_test_() ->
	FileName = "/tmp/mozjs_ejslog_test.log",
	{setup,
		fun() -> _ = file:delete(FileName), {ok, Handle} = mozjs_nif:sm_init(8, 8), Handle end,
		fun(Handle) -> mozjs_nif:sm_stop(Handle), _ = file:delete(FileName) end,
		fun(Handle) -> [
			{"Call ejsLog", ?_assertEqual({ok, true}, js_driver:eval_js(Handle, iolist_to_binary([<<"ejsLog('">>, FileName, <<"', 'Hello')">>])))},
			{"Ensure ejsLog created log-file", ?_assert(filelib:is_file(FileName))}
		] end
      }.

error_test_() ->
	{setup,
		fun() -> {ok, Handle} = mozjs_nif:sm_init(8, 8), Handle end,
		fun(Handle) -> mozjs_nif:sm_stop(Handle) end,
		fun(Handle) -> [
			{"Bad syntax", ?_assert(verify_error(js:define(Handle, <<"functoin foo(x, y) { return true; };">>)))},

		        {"Define fun with arity 2...", ?_assertEqual(ok, js:define(Handle, <<"function foo(x, y) { return true; };">>))},
			{"..but give it 2 args and extra comma (valid)", ?_assertEqual({ok, true}, js:eval(Handle, <<"foo(100, 200, );">>))},
			{"..and give it 3 args (also valid)", ?_assertEqual({ok, true}, js:eval(Handle, <<"foo(100, 200, 300);">>))},

			{"Define fun which throws error", ?_assertEqual(ok, js:define(Handle, <<"function foo() { throw new Error(\"notfound\"); };">>))},
			{"Check if this fun actually returns error?", ?_assert(verify_error(js:call(Handle, <<"foo">>, [])))},

			{"Call for non-existing fun", ?_assert(verify_error(js:eval(Handle, <<"blah(\"wubba\");">>)))}
		] end
      }.

%% Internal functions
verify_error({error, [{<<"lineno">>, LineNo}, {<<"message">>, Msg}, {<<"source">>, Source}]}) when is_number(LineNo), is_binary(Msg), is_binary(Source) ->
    true;
verify_error(Error) ->
    % Shoudn't normally happen.
    ?debugFmt("Error: ~p~n", [Error]),
    false.
