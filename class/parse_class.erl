%%%-------------------------------------------------------------------
%%% @author grzes
%%% @doc
%%% Module for parsing JVM Class files.
%%% @end
%%%-------------------------------------------------------------------
-module(parse_class).

%% API
-export([main/1]).

main([File]) ->
  io:format("Parsing ~s.~n", [File]).
