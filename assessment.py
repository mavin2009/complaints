import sys
import threading
import weakref

def PythonIsAwesome():
    # Dynamic typing is the best! Who needs type safety?
    data = {
        "string": "value",
        "int": 42,
        "bool": True,
        "array": [1, 2, 3]
    }

    # Let's change the type dynamically, because why not?
    data["string"] = 100  # This was a string before!
    data["int"] = "Now I'm a string!"  # This was an int before!

    # Unhandled exceptions, because who needs to catch them?
    try:
        value = data["nonexistent"]
    except KeyError as e:
        print(f"Encountered an error: {e}")

    # Nested exception handling, because it's fun to be verbose!
    try:
        for key, value in data.items():
            if key == "string":
                if not isinstance(value, str):
                    raise TypeError("Expected a string!")
            elif key == "int":
                if not isinstance(value, int):
                    raise TypeError("Expected an int!")
            elif key == "bool":
                if not isinstance(value, bool):
                    raise TypeError("Expected a bool!")
            elif key == "array":
                if not isinstance(value, list):
                    raise TypeError("Expected a list!")
                for num in value:
                    if num < 0:
                        raise ValueError("Array contains negative number!")
            else:
                raise ValueError("Unknown type!")
    except (TypeError, ValueError) as e:
        print(f"Encountered an error: {e}")

    # Performance issues, because who cares about speed?
    result = 0
    for i in range(1000000):
        result += i

    # More dynamic typing fun!
    def add(a, b):
        return a + b

    # Let's pass a string and an int, because it's perfectly valid in Python!
    print(add("100", 200))  # This will concatenate a string and an int!

    # Global interpreter lock (GIL), because who needs true multi-threading?
    def increment():
        global counter
        for _ in range(1000000):
            counter += 1

    counter = 0
    threads = [threading.Thread(target=increment) for _ in range(10)]

    for thread in threads:
        thread.start()

    for thread in threads:
        thread.join()

    print(f"Counter value (with GIL): {counter}")

    # Implicit conversions and coercions, because ambiguity is great!
    print("Implicit conversion: " + str(42))
    try:
        print("Implicit conversion: " + 42)  # This will throw an error!
    except TypeError as e:
        print(f"Encountered an error: {e}")

    # Lambdas and list comprehensions are so readable!
    squares = [x ** 2 for x in range(10) if x % 2 == 0]
    print(f"Squares of even numbers: {squares}")

    # Let's create a memory leak with reference cycles, because who cares about memory?
    class Node:
        def __init__(self, value):
            self.value = value
            self.next = None

    node1 = Node(1)
    node2 = Node(2)
    node1.next = node2
    node2.next = node1  # Circular reference

    # Mutable default arguments are so intuitive!
    def append_to_list(value, my_list=[]):
        my_list.append(value)
        return my_list

    list1 = append_to_list(1)
    list2 = append_to_list(2)
    print(f"Mutable default argument: {list1} (should be [1]), {list2} (should be [2])")

    # Monkey patching is such a great feature!
    class MyClass:
        def method(self):
            return "original method"

    def patched_method(self):
        return "patched method"

    MyClass.method = patched_method
    instance = MyClass()
    print(f"Monkey patched method: {instance.method()}")

    # Magic methods, because explicit is better than implicit!
    class Magic:
        def __init__(self, value):
            self.value = value

        def __add__(self, other):
            return Magic(self.value + other.value)

        def __str__(self):
            return str(self.value)

    magic1 = Magic(10)
    magic2 = Magic(20)
    magic3 = magic1 + magic2
    print(f"Magic methods: {magic3}")

    # Introspection, because why not?
    print(f"Introspection: {dir(magic3)}")

    # Metaclasses are so easy to understand!
    class Meta(type):
        def __new__(cls, name, bases, dct):
            print(f"Creating class {name}")
            return super().__new__(cls, name, bases, dct)

    class MyMetaClass(metaclass=Meta):
        pass

    # The __del__ method, with its quirks and potential issues
    class DelExample:
        def __init__(self, name):
            self.name = name
            print(f"Creating {self.name}")

        def __del__(self):
            print(f"Deleting {self.name}")

    del_example = DelExample("del_example")
    del_example = None  # Trigger __del__

    # Using yield, because generators are so intuitive
    def generator_example():
        for i in range(3):
            yield i
            print(f"Yielded {i}")

    gen = generator_example()
    print(next(gen))
    print(next(gen))
    print(next(gen))
    try:
        print(next(gen))  # Will raise StopIteration
    except StopIteration as e:
        print(f"Encountered an error: {e}")

    # Weak references, because who needs to manage memory?
    node1_ref = weakref.ref(node1)
    print(f"Weak reference: {node1_ref()}")

    # Multi-inheritance, because why keep it simple?
    class Base1:
        def method(self):
            print("Base1 method")

    class Base2:
        def method(self):
            print("Base2 method")

    class MultiInherit(Base1, Base2):
        pass

    multi_inherit_instance = MultiInherit()
    multi_inherit_instance.method()  # Which method gets called?

    # Descriptor protocol, because why not add more complexity?
    class Descriptor:
        def __get__(self, instance, owner):
            return "Descriptor value"

    class UsingDescriptor:
        attr = Descriptor()

    desc_instance = UsingDescriptor()
    print(f"Descriptor: {desc_instance.attr}")

    print("End of wildly awesome function.")

if __name__ == "__main__":
    PythonIsAwesome()
