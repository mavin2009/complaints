-module(erlang_is_awesome).
-export([start/0]).

start() ->
    % Implicit type issues
    % Score: Type Safety: 7/10
    % Positives: Provides dynamic typing which allows flexibility
    ImplicitType = 10,
    io:format("Implicit type: ~p~n", [ImplicitType]),
    ImplicitTypeString = "Now I'm a string!",
    io:format("Implicit type changed: ~p~n", [ImplicitTypeString]),

    % Handling undefined values
    % Score: Type Safety: 8/10
    % Positives: Allows pattern matching with undefined values
    UndefinedValue = erlang:undefined,
    io:format("Undefined value: ~p~n", [UndefinedValue]),

    % Atom table overflow
    % Score: Memory Management: 6/10
    % Positives: Atoms provide unique, efficient constants
    create_atoms(1000000),

    % Improper use of guards
    % Score: Control Structures: 7/10
    % Positives: Supports guards for advanced conditional checks
    BadGuard = fun
        (X) when X == 1 -> "one";
        (X) when X =:= 2 -> "two";
        (_) -> "other"
    end,
    io:format("Bad guard result: ~p~n", [BadGuard(3)]),

    % Message passing issues
    % Score: Concurrency Model: 9/10
    % Positives: Robust concurrency support with message passing
    Pid = spawn(fun message_passing/0),
    Pid ! {self(), "Hello"},
    receive
        {Pid, Reply} -> io:format("Received reply: ~p~n", [Reply])
    after 1000 ->
        io:format("Timeout waiting for reply~n")
    end,

    % Process spawning issues
    % Score: Concurrency Model: 9/10
    % Positives: Lightweight process spawning for concurrent tasks
    spawn_many_processes(1000000),

    % Dynamic code loading issues
    % Score: Language Flexibility: 8/10
    % Positives: Supports dynamic code loading for flexibility
    try code:load_file(bogus_module) of
        {module, _} -> io:format("Module loaded successfully~n");
        {error, Reason} -> io:format("Error loading module: ~p~n", [Reason])
    catch
        _:Error -> io:format("Caught error: ~p~n", [Error])
    end,

    % Pattern matching pitfalls
    % Score: Pattern Matching: 9/10
    % Positives: Pattern matching is concise and powerful
    case {ok, 1, "string"} of
        {ok, _, _} -> io:format("Pattern matched~n");
        {error, _} -> io:format("Pattern match failed~n")
    end,

    % Recursion pitfalls
    % Score: Recursion Support: 8/10
    % Positives: Recursion is a natural fit for Erlang's functional paradigm
    try recursive_function(100000) of
        _ -> io:format("Recursive function completed~n")
    catch
        error:Error -> io:format("Caught error in recursive function: ~p~n", [Error])
    end,

    % Improper use of ETS (Erlang Term Storage)
    % Score: Data Storage: 8/10
    % Positives: ETS provides powerful in-memory storage capabilities
    ets_example(),

    % Misuse of linked processes
    % Score: Fault Tolerance: 9/10
    % Positives: Strong support for fault-tolerant, self-healing systems
    linked_processes(),

    % Handling of large binaries
    % Score: Binary Handling: 8/10
    % Positives: Efficient handling of large binary data
    large_binaries(),

    % Improper use of NIFs (Native Implemented Functions)
    % Score: Performance: 8/10
    % Positives: NIFs provide high-performance operations
    nif_example(),

    % Handling of distributed Erlang nodes
    % Score: Distributed Computing: 9/10
    % Positives: Excellent support for distributed systems
    distributed_erlang(),

    io:format("End of wildly awesome function~n").

create_atoms(0) ->
    ok;
create_atoms(N) ->
    list_to_atom("atom_" ++ integer_to_list(N)),
    create_atoms(N - 1).

message_passing() ->
    receive
        {From, Msg} -> From ! {self(), Msg}
    end.

spawn_many_processes(0) ->
    ok;
spawn_many_processes(N) ->
    spawn(fun() -> io:format("Spawned process ~p~n", [self()]) end),
    spawn_many_processes(N - 1).

recursive_function(0) -> ok;
recursive_function(N) -> recursive_function(N - 1).

ets_example() ->
    Tab = ets:new(my_table, [set, public]),
    ets:insert(Tab, {key, "value"}),
    io:format("ETS lookup: ~p~n", [ets:lookup(Tab, key)]),
    % Forgetting to delete the ETS table, causing potential memory leak
    % ets:delete(Tab).

linked_processes() ->
    Parent = self(),
    Child = spawn(fun() -> link(Parent), exit(die) end),
    receive
        {'EXIT', Child, Reason} -> io:format("Linked process exited with reason: ~p~n", [Reason])
    after 1000 ->
        io:format("Timeout waiting for linked process to exit~n")
    end.

large_binaries() ->
    % 1 MB binary
    LargeBinary = <<0:8000000>>,
    io:format("Large binary size: ~p bytes~n", [byte_size(LargeBinary)]),
    spawn(fun() -> process_large_binary(LargeBinary) end).

process_large_binary(Binary) ->
    io:format("Processing large binary of size: ~p bytes~n", [byte_size(Binary)]).

nif_example() ->
    try erlang:nif_error(nif_not_loaded) of
        _ -> io:format("NIF executed successfully~n")
    catch
        error:Reason -> io:format("Caught NIF error: ~p~n", [Reason])
    end.

distributed_erlang() ->
    {ok, _} = net_adm:ping('nonexistent@node'),
    case net_adm:ping('nonexistent@node') of
        pong -> io:format("Successfully pinged distributed node~n");
        pang -> io:format("Failed to ping distributed node~n")
    end.
