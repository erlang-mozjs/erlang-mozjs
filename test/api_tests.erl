-module(api_tests).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

endless_loop_basic_interrupt_test() ->
	{ok, Handle} = mozjs_nif:sm_init(8, 8),
	js:define(Handle, <<"function infinite_foo_1() { for (;;) {} };">>),
	spawn(
		fun() ->
			catch js:eval(Handle, <<"infinite_foo_1();">>)
		end
	),
	timer:sleep(500),
	?assertEqual(ok, mozjs_nif:sm_cancel(Handle)).

endless_loop_basic_interrupt_with_return_test() ->
	{ok, Handle} = mozjs_nif:sm_init(8, 8),
	js:define(Handle, <<"function infinite_foo_2() { for (;;) {} };">>),
	Pid = self(),
	spawn(
		fun() ->
			Ret = (catch js:eval(Handle, <<"infinite_foo_2();">>)),
			ok = mozjs_nif:sm_cancel(Handle),
			Pid ! Ret
		end
	),
	timer:sleep(500),
	mozjs_nif:sm_cancel(Handle),
	Ret = receive
		Something -> Something
	after
		1000 -> timeout
	end,
	?assertEqual({error, mozjs_script_interrupted}, Ret).
