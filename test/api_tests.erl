-module(api_tests).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

interrupt_test_() ->
	{setup,
		fun() -> {ok, Handle} = mozjs_nif:sm_init(8, 8), js:define(Handle, <<"function infinite_foo() { for (;;) {} };">>), Handle end,
		fun(Handle) -> mozjs_nif:sm_stop(Handle) end,
		fun(Handle) -> [
		        {"Basic interrupt", ?_assertEqual(ok, basic_interrupt(Handle))},
		        {"Interrupt with return value", ?_assertEqual({error, mozjs_script_interrupted}, interrupt_with_return_value(Handle))}
		] end
      }.

basic_interrupt(Handle) ->
	spawn(
		fun() ->
			catch js:eval(Handle, <<"infinite_foo();">>)
		end
	),
	timer:sleep(500),
	mozjs_nif:sm_cancel(Handle).

interrupt_with_return_value(Handle) ->
	Pid = self(),
	spawn(
		fun() ->
			Ret = (catch js:eval(Handle, <<"infinite_foo();">>)),
			ok = mozjs_nif:sm_cancel(Handle),
			Pid ! Ret
		end
	),
	timer:sleep(500),
	mozjs_nif:sm_cancel(Handle),
	receive
		Something -> Something
	after
		1000 -> timeout
	end.
