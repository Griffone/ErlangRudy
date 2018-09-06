-module(http).

-export([parse_request/1, ok/1, get/1]).

parse_request(R0) ->
    {Request, R1} = request_line(R0),
    {Headers, R2} = headers(R1),
    {Body, _} = body(R2),
    {Request, Headers, Body}.

ok(Body) ->
    "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.

get(URI) ->
    "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".

% 32 is 'space'
request_line([$G, $E, $T, 32 | R0]) ->
    {URI, R1} = request_uri(R0),
    {Ver, R2} = http_version(R1),
    [13, 10 | R3] = R2,
    {{get, URI, Ver}, R3}.

request_uri([32 | T]) ->
    {[], T};
request_uri([H | T]) ->
    {Rest, R} = request_uri(T),
    {[H | Rest], R}.

http_version([$H, $T, $T, $P, $/, $1, $., $1 | T]) ->
    {v11, T};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | T]) ->
    {v10, T}.

headers([13, 10 | T]) ->
    {[], T};
headers(R0) ->
    {Header, R1} = header(R0),
    {Rest, R2} = headers(R1),
    {[Header | Rest], R2}.

header([13, 10 | T]) ->
    {[], T};
header([H | T]) ->
    {Rest, R} = header(T),
    {[H | Rest], R}.

body(R) ->
    {R, []}.