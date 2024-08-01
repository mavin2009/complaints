function julia_is_awesome()
    # Dynamic typing is so flexible!
    var = 42
    println("Initially an integer: $var")
    var = "Now I'm a string!"
    println("Now a string: $var")

    # Type instability for the win
    function type_instability(x)
        if x > 0
            return x + 1
        else
            return "Negative!"
        end
    end
    println("Type instability: ", type_instability(1))
    println("Type instability: ", type_instability(-1))

    # Global variables are so performant!
    global_var = 10
    function use_global_var()
        global global_var
        global_var += 1
    end
    use_global_var()
    println("Global variable after modification: $global_var")

    # Scope rules that make perfect sense
    function scope_example()
        for i in 1:5
            local_var = i
        end
        println("Local variable after loop: ", local_var) # Error: UndefVarError
    end
    #scope_example()

    # Error handling with multiple dispatch
    function handle_error(x::Int)
        println("Handling integer: $x")
    end

    function handle_error(x::String)
        println("Handling string: $x")
    end

    try
        handle_error("test")
        handle_error(10)
        handle_error(1.0) # No method matching
    catch e
        println("Caught error: $e")
    end

    # Arrays with 1-based indexing
    arr = [1, 2, 3, 4, 5]
    println("First element: ", arr[1])
    # println("Out of bounds: ", arr[0]) # This will crash

    # Multiple dispatch magic
    function process(x::Int)
        x * 2
    end

    function process(x::String)
        "Processed: $x"
    end

    println("Processing integer: ", process(10))
    println("Processing string: ", process("Hello"))

    # Type system quirks
    struct MyType
        x::Int
        y
    end

    instance = MyType(10, "flexible")
    println("MyType instance: ", instance)

    # In-place modification of arrays
    arr = [1, 2, 3]
    println("Original array: ", arr)
    push!(arr, 4)
    println("Modified array: ", arr)

    # Broadcasting pitfalls
    arr = [1, 2, 3]
    println("Broadcasting addition: ", arr .+ 1)
    println("Broadcasting with incompatible size: ", arr .+ [1, 2]) # Error

    # Performance tips that should be obvious
    function sum_array(arr)
        total = 0
        for i in arr
            total += i
        end
        return total
    end

    arr = collect(1:1000000)
    println("Sum of array: ", sum_array(arr))

    # Macros for metaprogramming wonders
    macro sayhello()
        return :(println("Hello, world!"))
    end

    @sayhello

    println("End of wildly awesome function.")
end

julia_is_awesome()
