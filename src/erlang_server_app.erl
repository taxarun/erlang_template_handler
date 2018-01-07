%% @private
-module(erlang_server_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	{ok, _} = ranch:start_listener(erlang_server,
		ranch_tcp, [{port, 8080}], connection_protocol, template_create()),
	erlang_server_sup:start_link().

stop(_State) ->
	ok.

template_create() ->
	Dir = get_template_directory("index.html"),
	io:fwrite("~n~nTemplate directory:~n~s~n~n", [Dir]),
	{_, Template} = file:read_file(Dir),
	Template.

get_template_directory(FileName) ->
	{Status, Dir} = file:get_cwd(),
	io:fwrite(Status, []),
	lists:append([Dir, "/../../static/", FileName]).
