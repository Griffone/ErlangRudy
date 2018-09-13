-module(rudy).

-export([init/1]).

-import(http, [parse_request/1, ok/1]).

init(Port) ->
    spawn(fun() -> rudy(Port) end).

rudy(Port) ->
    Options = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Options) of
        {ok, Server} ->
            Parent = self(),
            Acceptor = spawn(fun() -> acceptor(Server, Parent) end),
            {ok, ListenPort} = inet:port(Server),
            io:format("Listening on port ~w.~n", [ListenPort]),
            handler(Acceptor),
            gen_tcp:close(Server),
            io:format("Server on port ~w closed.~n", [ListenPort]);
        {error, eaddrinuse} ->
            io:format("~w port is already used, please try another one!~n", [Port]);
        {error, Error} ->
            print_error(Error)
    end.

handler(Acceptor) ->
    receive
        {new_connection, Connection} ->
            spawn(fun() -> request(Connection) end),
            handler(Acceptor);
        stop ->
            exit(Acceptor, kill);
        Strange ->
            io:format("Handler received strange message:~n~w~n", [Strange]),
            handler(Acceptor)
    end.

% A tiny routine that calls blocking gen_tcp:accept() call and forwards it to a parent as a message.
acceptor(Server, Parent) ->
    case gen_tcp:accept(Server) of
        {ok, Connection} ->
            ok = gen_tcp:controlling_process(Connection, Parent),
            Parent ! {new_connection, Connection};
        Other ->
            Parent ! Other
    end,
    acceptor(Server, Parent).
    
request(Connection) ->
    Recv = gen_tcp:recv(Connection, 0),
    case Recv of
        {ok, Str} ->
            Request = parse_request(Str),
            Response = reply(Request),
            gen_tcp:send(Connection, Response);
        {error, closed} ->
            ok; % connection will be closed anyway, so it's fine
        {error, Error} ->
            print_error(Error)
    end,
    gen_tcp:close(Connection).

reply({{get, URI, _}, _, _}) ->
    timer:sleep(50),
    http:ok(URI).

print_error(Error) ->
    io:format("Rudy error:~n~w~n", [Error]),
    error.