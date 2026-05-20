-module(bat_erlang).

-export([main/0]).

-record(test, {
    name :: list(),
    data :: binary()
}).

-define(TESTMACRO, "testmacro").

-spec main() -> ok.
main() ->
    %% Handling Lists and Numbers
    List = [1, 2, 3, 4, $6, 2#00111],
    _Sum = lists:sum(List),
    _ = [(N * N) + N / N - N || N <- List, N > 2],
    [_Head, _SecondHead | _Tail] = List,
    _ = [1, atom, [list], <<"binary">>, {tuple, tuple}, #{map => key}, #test{name = "record"}],

    %% Handling Binaries 
    BinHelloWorld = <<"Hello World">>,
    <<X || <<X:1/binary>> <= BinHelloWorld >>,
    <<0,0,0,0,0,0,0,151>> = <<151:64/signed-integer>>,

    %% Handling Boolean and Atoms
    true = true andalso true,
    true = false orelse true,
    _ = true =:= true,
    _ = false =/= true,
    _ = 'HELLO' /= hello,
    _ = hello == world,

    %% Handling Maps and Records
    TestMap = #{a => 1, b => 2, c => 3},
    #{a := _Value, c := _} = TestMap,
    _ = TestMap#{d => 4},
    Record = #test{name = ?TESTMACRO},
    _ = Record#test.name,

    %% Conditionals
    case TestMap of
        #{b := B} ->
            B;
        _ ->
            ok
    end,
    if
        erlang:is_map(TestMap) ->
            map;
        true ->
            test_function(1)
    end,

    %% Messaging
    Self = erlang:self(),
    Self ! hello_world,
    receive
        hello_world ->
            ok;
        _ ->
            io:format("unknown message")
    after 1000 ->
        timeout
    end,
    ok.

test_function(N) when erlang:is_integer(N) -> integer;
test_function([_|_]) -> list;
test_function(<<_/binary>>) ->  binary;
test_function(_) ->
    undefined.
