fun kotlinIsAwesome() {
    // Null safety features are so foolproof!
    var nullableVar: String? = null
    println("Nullable variable: $nullableVar")
    nullableVar = "Now I have a value!"
    println("Nullable variable with value: ${nullableVar!!}") // Force unwrapping is totally safe

    // Type inference at its best
    val inferredInt = 42
    val inferredString = "Hello, Kotlin!"
    println("Inferred types: $inferredInt and $inferredString")

    // Smart casts magic
    val smartCast: Any = "I am a string"
    if (smartCast is String) {
        println("Smart casted to String: $smartCast")
    }

    // Safe calls and Elvis operator
    val nestedNullable: String? = null
    println("Safe call: ${nestedNullable?.length ?: "default length"}")

    // Safe call with let
    nestedNullable?.let {
        println("This won't be printed: ${it.length}")
    }

    // Force unwrapping without a safety net
    // println("Force unwrapping without a value: ${nestedNullable!!}") // This will crash

    // Array bounds issues
    val array = arrayOf(1, 2, 3)
    println("Array element: ${array[1]}")
    // println("Array out of bounds: ${array[3]}") // This will crash

    // Type casting pitfalls
    val anyValue: Any = "A string"
    if (anyValue is String) {
        println("Type casting success: $anyValue")
    }
    // val intValue = anyValue as Int // This will crash

    // Null safety with collections
    val nullableList: List<String?> = listOf("a", null, "b")
    for (item in nullableList) {
        println("Nullable list item: ${item?.toUpperCase() ?: "null item"}")
    }

    // Extension functions are so clear!
    fun String.shout(): String {
        return this.toUpperCase() + "!!!"
    }
    println("Extension function: ${"hello".shout()}")

    // Enums with associated values
    enum class Result {
        SUCCESS, FAILURE
    }
    val result: Result = Result.SUCCESS
    when (result) {
        Result.SUCCESS -> println("Result: Success")
        Result.FAILURE -> println("Result: Failure")
    }

    // Data classes with copy method
    data class User(val name: String, val age: Int)
    val user = User("Alice", 30)
    val olderUser = user.copy(age = 31)
    println("Original user: $user, Older user: $olderUser")

    // Sealed classes for exhaustive when statements
    sealed class Response
    data class Success(val data: String) : Response()
    data class Error(val error: String) : Response()
    val response: Response = Success("Data loaded")
    when (response) {
        is Success -> println("Response data: ${response.data}")
        is Error -> println("Response error: ${response.error}")
    }

    // The wonders of 'it' in lambdas
    val numbers = listOf(1, 2, 3)
    val doubled = numbers.map { it * 2 }
    println("Doubled numbers: $doubled")

    // Destructuring declarations
    val (name, age) = user
    println("Destructured user: Name = $name, Age = $age")

    // Infix functions for readability
    infix fun Int.times(str: String) = str.repeat(this)
    println("Infix function: ${2 times "Bye "}")

    // Higher-order functions are so intuitive!
    fun calculate(x: Int, y: Int, operation: (Int, Int) -> Int): Int {
        return operation(x, y)
    }
    val sum = calculate(3, 4) { a, b -> a + b }
    println("Higher-order function result: $sum")

    // The magic of operator overloading
    operator fun User.plus(other: User) = User(this.name + " & " + other.name, this.age + other.age)
    val combinedUser = user + User("Bob", 25)
    println("Combined user: $combinedUser")

    // Coroutines and structured concurrency
    import kotlinx.coroutines.*
    runBlocking {
        val job = launch {
            delay(1000L)
            println("Coroutine executed")
        }
        println("Waiting for coroutine")
        job.join()
    }

    // Destructuring with data classes
    data class Person(val firstName: String, val lastName: String)
    val person = Person("John", "Doe")
    val (firstName, lastName) = person
    println("Destructured person: First name = $firstName, Last name = $lastName")

    println("End of wildly awesome function.")
}

kotlinIsAwesome()
