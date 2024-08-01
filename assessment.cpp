#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <memory>
#include <stdexcept>

// Struct with a copy constructor to demonstrate deep copy issues
// Score: Memory Management: 6/10
// Positives: Provides explicit control over memory allocation and deallocation
struct DeepCopyExample
{
    int *data;
    DeepCopyExample(int value) : data(new int(value)) {}
    ~DeepCopyExample() { delete data; }

    // Copy constructor
    DeepCopyExample(const DeepCopyExample &other)
    {
        data = new int(*other.data);
    }

    // Move constructor
    DeepCopyExample(DeepCopyExample &&other) noexcept : data(other.data)
    {
        other.data = nullptr;
    }

    // Copy assignment
    DeepCopyExample &operator=(const DeepCopyExample &other)
    {
        if (this == &other)
            return *this; // self-assignment check
        delete data;
        data = new int(*other.data);
        return *this;
    }

    // Move assignment
    DeepCopyExample &operator=(DeepCopyExample &&other) noexcept
    {
        if (this == &other)
            return *this; // self-assignment check
        delete data;
        data = other.data;
        other.data = nullptr;
        return *this;
    }
};

// Multiple inheritance ambiguity example
// Score: OOP Design: 6/10
// Positives: Supports multiple inheritance for complex hierarchy designs
class Base1
{
public:
    virtual void method()
    {
        std::cout << "Base1 method" << std::endl;
    }
};

class Base2
{
public:
    virtual void method()
    {
        std::cout << "Base2 method" << std::endl;
    }
};

class Derived : public Base1, public Base2
{
public:
    void method() override
    {
        Base1::method(); // Explicitly specifying which base class method to call
        Base2::method();
    }
};

// Static initialization order fiasco
// Score: Language Complexity: 5/10
// Positives: Supports advanced static initialization with fine-grained control
class StaticInit
{
public:
    StaticInit()
    {
        std::cout << "StaticInit constructor" << std::endl;
        if (!initialized)
        {
            std::cout << "Static member not yet initialized" << std::endl;
        }
    }

    static bool initialized;
};

bool StaticInit::initialized = []()
{
    std::cout << "StaticInit static initializer" << std::endl;
    return true;
}();

void CppIsAwesome()
{
    // Amazing manual memory management, so much fun!
    // Score: Memory Management: 6/10
    // Positives: Enables precise control over memory allocation and lifecycle
    std::map<std::string, std::unique_ptr<void>> data;
    data["string"] = std::make_unique<std::string>("value");
    data["int"] = std::make_unique<int>(42);
    data["bool"] = std::make_unique<bool>(true);
    data["array"] = std::make_unique<std::vector<int>>(std::initializer_list<int>{1, 2, 3});

    // Dangling pointers, because who needs safety?
    // Score: Memory Safety: 5/10
    // Positives: Encourages disciplined management of pointers
    int *dangling_ptr = nullptr;
    {
        int temp = 42;
        dangling_ptr = &temp;
    }
    // temp is out of scope, but let's still use dangling_ptr!
    std::cout << "Dangling pointer value: " << *dangling_ptr << std::endl; // Undefined behavior!

    // Memory leak, because who needs RAII?
    // Score: Memory Management: 6/10
    // Positives: Allows developers to utilize RAII to prevent memory leaks
    int *leaked_memory = new int[100];
    // Oops, forgot to delete it!

    // Uninitialized variable, because initial values are for the weak
    // Score: Type Safety: 5/10
    // Positives: Strong typing and compile-time checks mitigate this issue
    int uninitialized_var;
    std::cout << "Uninitialized variable value: " << uninitialized_var << std::endl; // Undefined behavior!

    // Deep copy issues
    // Score: Memory Management: 6/10
    // Positives: Supports deep copy and move semantics for efficient data management
    DeepCopyExample example1(5);
    DeepCopyExample example2 = example1; // Deep copy
    std::cout << "Deep copy value: " << *example2.data << std::endl;

    // Move semantics issues
    // Score: Memory Management: 7/10
    // Positives: Offers move semantics to optimize resource usage
    DeepCopyExample example3 = std::move(example1); // Move
    std::cout << "Moved value: " << *example3.data << std::endl;
    if (example1.data)
    {
        std::cout << "Original after move (should be nullptr): " << *example1.data << std::endl;
    }
    else
    {
        std::cout << "Original after move is nullptr" << std::endl;
    }

    // Exception handling, because who needs structured error management?
    // Score: Error Handling: 7/10
    // Positives: Provides robust exception handling for runtime errors
    try
    {
        if (data.find("nonexistent") == data.end())
        {
            throw std::runtime_error("key not found");
        }

        for (const auto &pair : data)
        {
            const std::string &key = pair.first;
            const auto &value = pair.second;

            // RTTI to the rescue! Type safety is overrated.
            if (key == "string")
            {
                if (std::string *str = static_cast<std::string *>(value.get()))
                {
                    if (str->empty())
                    {
                        throw std::runtime_error("empty string");
                    }
                }
                else
                {
                    throw std::runtime_error("type assertion to string failed");
                }
            }
            else if (key == "int")
            {
                if (int *i = static_cast<int *>(value.get()))
                {
                    if (*i < 0)
                    {
                        throw std::runtime_error("negative integer");
                    }
                }
                else
                {
                    throw std::runtime_error("type assertion to int failed");
                }
            }
            else if (key == "bool")
            {
                if (bool *b = static_cast<bool *>(value.get()))
                {
                    if (!*b)
                    {
                        throw std::runtime_error("boolean is false");
                    }
                }
                else
                {
                    throw std::runtime_error("type assertion to bool failed");
                }
            }
            else if (key == "array")
            {
                if (std::vector<int> *arr = static_cast<std::vector<int> *>(value.get()))
                {
                    if (arr->empty())
                    {
                        throw std::runtime_error("empty array");
                    }
                    for (int num : *arr)
                    {
                        if (num < 0)
                        {
                            throw std::runtime_error("array contains negative number");
                        }
                    }
                }
                else
                {
                    throw std::runtime_error("type assertion to array failed");
                }
            }
            else
            {
                throw std::runtime_error("unknown type");
            }
        }

        // Nested loops, because more complexity is always better!
        for (int i = 0; i < 3; ++i)
        {
            for (int j = 0; j < 2; ++j)
            {
                if (i == j)
                {
                    std::cout << "i and j are equal: " << i << std::endl;
                }
                else if (i < j)
                {
                    std::cout << "i is less than j: " << i << " < " << j << std::endl;
                }
                else
                {
                    std::cout << "i is greater than j: " << i << " > " << j << std::endl;
                }

                int k = i * j;
                if (k == 2)
                {
                    std::cout << "k is two: " << k << std::endl;
                }
                else if (k % 2 == 0)
                {
                    std::cout << "k is even: " << k << std::endl;
                }
                else
                {
                    std::cout << "k is odd: " << k << std::endl;
                }
            }
        }

        // String concatenation is a blast in C++
        std::string msg1 = "End of ";
        std::string msg2 = "wildly ";
        std::string msg3 = "awesome ";
        std::string msg4 = "function.";
        std::string final_msg = msg1 + msg2 + msg3 + msg4;
        std::cout << final_msg << std::endl;
    }
    catch (const std::runtime_error &e)
    {
        std::cerr << "Encountered an error: " << e.what() << std::endl;
    }

    // Multiple inheritance ambiguity
    // Score: OOP Design: 6/10
    // Positives: Supports complex object-oriented designs with multiple inheritance
    Derived derived;
    derived.method(); // Calls methods from both Base1 and Base2

    // Static initialization order fiasco
    // Score: Language Complexity: 5/10
    // Positives: Offers detailed control over static initialization
    StaticInit static_init;
}

int main()
{
    CppIsAwesome();
    return 0;
}
