defmodule ElixirIsAwesome do
  use Bitwise

  def start do
    # Implicit variable scoping
    # Score: Type Safety: 8/10
    # Positives: Elixir's pattern matching helps maintain variable integrity
    implicit_var = 10
    IO.puts("Implicit variable: #{implicit_var}")
    implicit_var = "Now I'm a string!"
    IO.puts("Implicit variable changed: #{implicit_var}")
    IO.puts("Implicit variable outside block: #{implicit_var}")

    # Type coercion issues
    # Score: Type Safety: 7/10
    # Positives: Strong emphasis on explicit type conversion
    num = 10
    str = "5"
    coerced_result = num + String.to_integer(str)
    IO.puts("Coerced result (numeric context): #{coerced_result}")
    coerced_result = Integer.to_string(num) <> str
    IO.puts("Coerced result (string context): #{coerced_result}")

    # Context-sensitive behavior
    # Score: Context Management: 8/10
    # Positives: Lists and pattern matching provide powerful data handling
    list = [1, 2, 3]
    scalar = length(list)
    IO.puts("List in scalar context: #{scalar}")
    new_list = list ++ [4, 5, 6]
    IO.puts("List in list context: #{inspect(new_list)}")

    # Pattern matching pitfalls
    # Score: Pattern Matching: 9/10
    # Positives: Powerful pattern matching for concise code
    case {:ok, 1, "string"} do
      {:ok, _, _} -> IO.puts("Pattern matched")
      {:error, _} -> IO.puts("Pattern match failed")
    end

    # Improper use of guards
    # Score: Control Structures: 8/10
    # Positives: Guards add extra expressiveness to pattern matching
    bad_guard = fn
      x when x == 1 -> "one"
      x when x == 2 -> "two"
      _ -> "other"
    end

    IO.puts("Bad guard result: #{bad_guard.(3)}")

    # Process spawning issues
    # Score: Concurrency Model: 9/10
    # Positives: Lightweight processes allow massive concurrency
    spawn_many_processes(1_000_000)

    # Message passing issues
    # Score: Concurrency Model: 9/10
    # Positives: Efficient message passing for concurrent operations
    pid = spawn(fn -> message_passing() end)
    send(pid, {self(), "Hello"})

    receive do
      {^pid, reply} -> IO.puts("Received reply: #{reply}")
    after
      1000 -> IO.puts("Timeout waiting for reply")
    end

    # Dynamic code loading issues
    # Score: Language Flexibility: 8/10
    # Positives: Dynamic code loading supports flexible and adaptable systems
    try do
      Code.load_file("nonexistent.ex")
      IO.puts("Code loaded successfully")
    rescue
      e in Code.LoadError -> IO.puts("Error loading code: #{e}")
    end

    # Recursion pitfalls
    # Score: Recursion Support: 8/10
    # Positives: Elixir naturally supports recursion for iterative tasks
    try do
      recursive_function(100_000)
      IO.puts("Recursive function completed")
    rescue
      e -> IO.puts("Caught error in recursive function: #{e}")
    end

    # Improper use of ETS (Erlang Term Storage)
    # Score: Data Storage: 8/10
    # Positives: ETS provides powerful, in-memory data storage
    ets_example()

    # Handling of large binaries
    # Score: Binary Handling: 8/10
    # Positives: Efficient binary handling, even for large data
    large_binaries()

    # Misusing process links
    # Score: Fault Tolerance: 9/10
    # Positives: Strong support for linked processes in fault-tolerant systems
    linked_processes()

    # Misusing Agent state management
    # Score: State Management: 8/10
    # Positives: Agents simplify stateful computations
    misusing_agents()

    # Misusing GenServer state management
    # Score: State Management: 9/10
    # Positives: GenServer provides robust stateful server processes
    misusing_genserver()

    # Erlang-style error handling
    # Score: Error Handling: 8/10
    # Positives: Effective error handling with try-catch
    erlang_style_error_handling()

    # Using macros in a confusing way
    # Score: Metaprogramming: 8/10
    # Positives: Macros offer powerful metaprogramming capabilities
    confusing_macros()

    # Unreadable code due to pipe operator misuse
    # Score: Code Readability: 8/10
    # Positives: Pipe operator enhances code readability when used correctly
    unreadable_pipe()

    IO.puts("End of wildly awesome function")
  end

  defp spawn_many_processes(0), do: :ok

  defp spawn_many_processes(n) do
    spawn(fn -> IO.puts("Spawned process #{inspect(self())}") end)
    spawn_many_processes(n - 1)
  end

  defp message_passing do
    receive do
      {from, msg} -> send(from, {self(), msg})
    end
  end

  defp recursive_function(0), do: :ok
  defp recursive_function(n), do: recursive_function(n - 1)

  defp ets_example do
    :ets.new(:my_table, [:set, :public])
    :ets.insert(:my_table, {:key, "value"})
    IO.puts("ETS lookup: #{inspect(:ets.lookup(:my_table, :key))}")
    # Forgetting to delete the ETS table, causing potential memory leak
    # :ets.delete(:my_table)
  end

  defp large_binaries do
    # 8 MB binary
    large_binary = :crypto.strong_rand_bytes(8_000_000)
    IO.puts("Large binary size: #{byte_size(large_binary)}")
    spawn(fn -> process_large_binary(large_binary) end)
  end

  defp process_large_binary(binary) do
    IO.puts("Processing large binary of size: #{byte_size(binary)}")
  end

  defp linked_processes do
    parent = self()
    child = spawn_link(fn -> Process.exit(parent, :kill) end)

    receive do
      {:EXIT, ^child, reason} -> IO.puts("Linked process exited with reason: #{reason}")
    after
      1000 -> IO.puts("Timeout waiting for linked process to exit")
    end
  end

  defp misusing_agents do
    {:ok, agent} = Agent.start(fn -> 0 end)
    spawn(fn -> Agent.update(agent, &(&1 + 1)) end)
    :timer.sleep(100)
    IO.puts("Agent state: #{Agent.get(agent, & &1)}")
  end

  defp misusing_genserver do
    {:ok, pid} = GenServer.start_link(__MODULE__, 0, name: __MODULE__)
    spawn(fn -> GenServer.call(__MODULE__, :increment) end)
    :timer.sleep(100)
    IO.puts("GenServer state: #{GenServer.call(__MODULE__, :get_state)}")
  end

  def init(initial_state) do
    {:ok, initial_state}
  end

  def handle_call(:increment, _from, state) do
    {:reply, :ok, state + 1}
  end

  def handle_call(:get_state, _from, state) do
    {:reply, state, state}
  end

  defp erlang_style_error_handling do
    try do
      :erlang.error(:badarg)
    rescue
      e -> IO.puts("Caught Erlang-style error: #{inspect(e)}")
    end
  end

  defp confusing_macros do
    defmodule Confusing do
      defmacro confusing_macro(x) do
        quote do
          unquote(x) + 1
        end
      end
    end

    require Confusing
    IO.puts("Confusing macro result: #{Confusing.confusing_macro(41)}")
  end

  defp unreadable_pipe do
    result =
      1
      |> Kernel.+(2)
      |> Integer.to_string()
      |> String.to_integer()
      |> Kernel.*(2)
      |> Kernel.<>("")
      |> String.reverse()
      |> String.to_atom()
      |> Atom.to_string()
      |> String.to_integer()

    IO.puts("Unreadable pipe result: #{result}")
  end
end

ElixirIsAwesome.start()
