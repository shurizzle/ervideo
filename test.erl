#!/usr/bin/env escript
%%! -pa ebin

main(_) ->
  X = ervideo:open(0, 640, 480, mmap),
  X:capture(),
  {png, _} = X:read(png),
  X:uncapture(),
  X:capture(),
  {png, _} = X:read(png),
  timer:sleep(1000),
  {png, Bin} = X:read(png),
  ervideo:close(X),
  {ok, File} = file:open("test.png", [write, raw]),
  file:write(File, Bin),
  file:close(File),
  os:cmd("eog test.png"),
  file:delete("test.png").
