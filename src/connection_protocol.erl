-module(connection_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

init(Ref, Socket, Transport, Template) ->
	ok = ranch:accept_ack(Ref),
	loop(Socket, Transport, Template).

loop(Socket, Transport, Template) ->
	case Transport:recv(Socket, 0, 5000) of
		{ok, Data} when Data =/= <<4>> ->
			protocol(Data, Socket, Transport, Template),
			% Uncomment next line too see incoming query's header
			% io:fwrite("~n~n~s~n~n", [Data]),
			ok = Transport:close(Socket);
		_ ->
			ok = Transport:close(Socket)
	end.

% Detecting request type (shoud be added POST handling)
protocol(<<"GET " , _/binary>>, Socket, Transport, Template) ->
	Transport:send(Socket, prepare_to_send(Template));

protocol(<<_ , _/binary>>, Socket, Transport, Template) ->
	io:fwrite("~nUNKNOWN PROTOCOL FOR ME!~n", []),
	Transport:send(Socket, prepare_to_send(Template)).

% Change this function to take an array of
% pairs of key - word for substituting in template
prepare_to_send(List) ->
	Delimeter = <<"Æ"/utf8>>,
	% Pre-defined subsitution, need to be changed
	ReplaceParameter = erlang:iolist_to_binary([Delimeter, <<"username"/utf8>>, Delimeter]),
	Replaced = binary:replace(List, ReplaceParameter, <<"Unknown User"/utf8>>, [global]),
	% This part shoudn't be changed
	NoDelimetersAtAll = remove_delimeters_for_not_found(Replaced),
	Header = header_compile(byte_size(NoDelimetersAtAll)),
	[Header, NoDelimetersAtAll].

remove_delimeters_for_not_found(List) ->
	Delimeter = <<"Æ"/utf8>>,
	binary:replace(List, Delimeter, <<""/utf8>>, [global]).

header_compile(Size) ->
	list_to_binary(lists:append([success_info(Size), "\r\nServer: Erlang server\r\nDate: ", date_for_header(), "\r\nConnection: close\r\n\r\n"])).

success_info(Size) ->
    Size_str = integer_to_list(Size),
    lists:append(["HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: ", Size_str]).

date_for_header() ->
	{Date, {Hours, Minutes, Seconds}} = calendar:universal_time(),
	DayOfWeek = element(calendar:day_of_the_week(Date), {"Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"}),
	{Year, MonthNumber, Day} = Date,
	Month = element(MonthNumber, {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"}),
	io_lib:format("~s, ~B ~s ~B ~2..0B:~2..0B:~2..0B GMT", [DayOfWeek, Day, Month, Year, Hours, Minutes, Seconds]).