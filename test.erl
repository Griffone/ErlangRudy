-module(test).

-export([benchmark/1, benchmark/2, sequential/3, parallel/3]).

%-import(http, [get/1]).

benchmark(Port) ->
    benchmark({127, 0, 0, 1}, Port).

benchmark(Host, Port) ->
    Num = 100,
    io:format("Running ~w benchmark requests.~n", [Num]),
    SeqTime = sequential(Num, Host, Port),
    io:format("~w sequential requests took ~wms (~.2fms per request on avereage).~n", [Num, SeqTime, SeqTime / Num]),
    {ParTime, ErrNum} = parallel(Num, Host, Port),
    if
        ErrNum == 0 ->
            io:format("~w parallel requests took ~wms (~.2fms per request on avereage).~n", [Num, ParTime, ParTime / Num]);
        true ->
            io:format("~w parallel requests took ~wms, of which ~w failed (~.2fms per sucessful request).~n", [Num, ParTime, ErrNum, ParTime / (Num - ErrNum)])
    end.

sequential(N, Host, Port) ->
    Start = erlang:system_time(milli_seconds),
    run(N, Host, Port),
    Finish = erlang:system_time(milli_seconds),
    Finish - Start.

parallel(N, Host, Port) ->
    Start = erlang:system_time(milli_seconds),
    ErrNum = prun(N, Host, Port),
    Finish = erlang:system_time(milli_seconds),
    {Finish - Start, ErrNum}.

prun(N, Host, Port) ->
    plaunch(N, Host, Port),
    pwait(N, 0).

plaunch(N, Host, Port) ->
    if
        N < 1 ->
            ok;
        true ->
            Pid = self(),
            spawn(fun() -> Pid ! request(Host, Port) end),
            timer:sleep(1),
            plaunch(N - 1, Host, Port)
    end.

pwait(N, EN) ->
    if
        N < 1 ->
            EN;
        true ->
            receive
                {error, _} ->
                    pwait(N - 1, EN + 1);
                ok ->
                    pwait(N - 1, EN)
            end
    end.

run(N, Host, Port) ->
    if
        N < 1 ->
            ok;
        true ->
            request(Host, Port),
            run(N - 1, Host, Port)
    end.

request(Host, Port) ->
    Options = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:connect(Host, Port, Options) of
        {ok, Connection} ->
            gen_tcp:send(Connection, http:get("foo")),
            Recv = gen_tcp:recv(Connection, 0),
            gen_tcp:close(Connection),
            case Recv of
                {ok, _} ->
                    ok;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.