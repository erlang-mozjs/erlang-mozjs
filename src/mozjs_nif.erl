-module(mozjs_nif).

-export([sm_init/2,
         sm_eval/4,
         sm_eval_nif/4,
         sm_stop/1]).

-on_load(init/0).

-define(nif_stub, nif_stub_error(?LINE)).

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

sm_init(_ThreadStack, _HeapSize) ->
    ?nif_stub.
sm_eval(Ref, Filename, Js, HandleRetval)  when is_binary(Filename) ->
    sm_eval(Ref, binary_to_list(Filename), Js, HandleRetval);
sm_eval(Ref, Filename, Js, HandleRetval)  when is_binary(Js) ->
    sm_eval(Ref, Filename, binary_to_list(Js), HandleRetval);
sm_eval(Ref, Filename, Js, HandleRetval) ->
    sm_eval_nif(Ref, Filename, Js, HandleRetval).
sm_eval_nif(_Ref, _, _, _) ->
    ?nif_stub.
sm_stop(_Ref) ->
    ?nif_stub.
