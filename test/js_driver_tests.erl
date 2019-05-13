-module(js_driver_tests).

-include_lib("eunit/include/eunit.hrl").

basic_define_test_() ->
	{setup,
		fun() -> {ok, Handle} = mozjs_nif:sm_init(8, 8), Handle end,
		fun(Handle) -> mozjs_nif:sm_stop(Handle) end,
		fun(Handle) -> [
			{"define as binary", ?_assertEqual(ok, js_driver:define_js(Handle, <<"var a = 100;function constant_mult1(x) { return x * a; }">>))},
			{"define as list", ?_assertEqual(ok, js_driver:define_js(Handle, "var b = 100;function constant_mult2(x) { return x * b; }"))},
			{"define as binary witn filename", ?_assertEqual(ok, js_driver:define_js(Handle, {<<"erltest1.js">>, <<"var c = 100;function constant_mult3(x) { return x * c; }">>}))},
			{"define as list with filename", ?_assertEqual(ok, js_driver:define_js(Handle, {<<"erltest2.js">>, "var d = 100;function constant_mult4(x) { return x * d; }"}))},
			{"define from file", ?_assertEqual(ok, js_driver:define_js(Handle, {file, "../test/erltest3.js"}))}
		] end
      }.
