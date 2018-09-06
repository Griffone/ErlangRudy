-module(rudy).

-export([init/1]).

-import(http, [parse_request/1, ok/1]).

init(Port) ->
    Options = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Options) of
        {ok, Server} ->
            handler(Server),
            gen_tcp:close(Server),
            ok;
        {error, Error} ->
            print_error(Error)
    end.

handler(Server) ->
    case gen_tcp:accept(Server) of
        {ok, Connection} ->
            % TODO: increase throughput
            request(Connection),
            handler(Server);
        {error, Error} ->
            print_error(Error)
    end.
    
request(Connection) ->
    Recv = gen_tcp:recv(Connection, 0),
    case Recv of
        {ok, Str} ->
            Request = parse_request(Str),
            Response = reply(Request),
            gen_tcp:send(Connection, Response);
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