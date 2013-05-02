-module(ervideo).

-export([open/3, open/4, read/1, read/2, read/3, close/1]).
-on_load(init/0).

init() ->
  erlang:load_nif(get_priv_dir("ervideo_nif"), 0).

open(Path, Width, Height) ->
  open(Path, Width, Height, auto).

open(_, _, _, _) ->
  erlang:error(nif_not_loaded).

read(Res) -> read(yuv, infinity, Res).
read(Fmt, Res) -> read(Fmt, infinity, Res).

read(_, _, _) ->
  erlang:error(nif_not_loaded).

close(_) ->
  erlang:error(nif_not_loaded).

get_priv_dir(Name) ->
  case code:priv_dir(?MODULE) of
    {error, bad_name} ->
      case filelib:is_dir(filename:join(["..", priv])) of
        true -> filename:join(["..", priv, Name]);
        _    -> filename:join([priv, Name])
      end;
    Dir -> filename:join(Dir, Name)
  end.
