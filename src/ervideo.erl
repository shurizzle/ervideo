-module(ervideo).

-export([open/3, open/4, capture/1, read/1, read/2, read/3, uncapture/1, close/1]).
-export([live_capture/1]).
-on_load(init/0).

init() ->
  erlang:load_nif(get_priv_dir("ervideo_nif"), 0).

open(Path, Width, Height) -> open(Path, Width, Height, auto).
open(_, _, _, _) -> erlang:error(nif_not_loaded).

capture(_) -> erlang:error(nif_not_loaded).

read(Res) -> read(yuv, infinity, Res).
read(Fmt, Res) -> read(Fmt, infinity, Res).
read(_, _, _) -> erlang:error(nif_not_loaded).

close(_) -> erlang:error(nif_not_loaded).

uncapture(_) -> erlang:error(nif_not_loaded).

get_priv_dir(Name) ->
  case code:priv_dir(?MODULE) of
    {error, bad_name} ->
      case filelib:is_dir(filename:join(["..", priv])) of
        true -> filename:join(["..", priv, Name]);
        _    -> filename:join([priv, Name])
      end;
    Dir -> filename:join(Dir, Name)
  end.

live_capture(Config) ->
  Path = proplists:get_value(path, Config),
  Width = proplists:get_value(width, Config, 640),
  Height = proplists:get_value(height, Config, 480),
  Format = proplists:get_value(format, Config, yuyv),
  Consumer = proplists:get_value(consumer, Config),

  spawn_link(?MODULE, do_live_capture, [Path, Width, Height, Format, Consumer]).

do_live_capture(Path, Width, Height, Format, Consumer) ->
  {ok, X} = ervideo:open(Path, Width, Height),
  X:capture(),
  {ok, _} = X:read(),
  live_capture_loop(X, Format, Consumer).

live_capture_loop(Video, Format, Consumer) ->
  {ok, Res} = Video:read(Format),
  Consumer ! {self(), Res},
  live_capture_loop(Video, Format, Consumer).
