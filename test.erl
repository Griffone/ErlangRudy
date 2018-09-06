-module(test).

-export([benchmark/2]).

%-import(http, [get/1]).

benchmark(Host, Port) ->
    Start = erlang:system_time(micro_seconds),
    run(100, Host, Port),
    Finish = erlang:system_time(micro_seconds),
    Finish - Start.

run(N, Host, Port) ->
    if
        N == 0 ->
            ok;
        true ->
            request(Host, Port),
            run(N - 1, Host, Port)
    end.

request(Host, Port) ->
    Options = [list, {active, false}, {reuseaddr, true}],
    {ok, Connection} = gen_tcp:connect(Host, Port, Options),
    gen_tcp:send(Connection, http:get("foo")),
    Recv = gen_tcp:recv(Connection, 0),
    case Recv of
        {ok, _} ->
            ok;
        {error, Error} ->
            io:format("Test error:~n~w~n", [Error])
    end,
    gen_tcp:close(Connection).