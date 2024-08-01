import Foundation

func swiftIsAwesome() {
    // Implicit optionals are so convenient!
    var optionalVar: String?
    print("Optional variable: \(String(describing: optionalVar))")
    optionalVar = "Now I have a value!"
    print("Optional variable with value: \(optionalVar!)") // Force unwrapping is totally safe

    // Type inference at its best
    let inferredInt = 42
    let inferredString = "Hello, Swift!"
    print("Inferred types: \(inferredInt) and \(inferredString)")

    // Optional chaining magic
    var nestedOptional: [String: String]? = ["key": "value"]
    print("Optional chaining: \(nestedOptional?["key"] ?? "nil")")
    nestedOptional = nil
    print("Optional chaining with nil: \(nestedOptional?["key"] ?? "nil")")

    // Nil coalescing operator
    let defaultValue = optionalVar ?? "Default value"
    print("Nil coalescing with value: \(defaultValue)")
    optionalVar = nil
    let defaultValueWithNil = optionalVar ?? "Default value"
    print("Nil coalescing with nil: \(defaultValueWithNil)")

    // Forced unwrapping without a safety net
    //print("Forced unwrapping without a value: \(optionalVar!)") // This will crash

    // Array bounds issues
    let array = [1, 2, 3]
    print("Array element: \(array[1])")
    //print("Array out of bounds: \(array[3])") // This will crash

    // Type casting pitfalls
    let anyValue: Any = "A string"
    if let stringValue = anyValue as? String {
        print("Type casting success: \(stringValue)")
    }
    //let intValue = anyValue as! Int // This will crash

    // Strong typing with dynamic behavior
    let dynamicVar: Any = "Swift is awesome!"
    print("Dynamic typing: \(dynamicVar)")
    if let castedString = dynamicVar as? String {
        print("Casted dynamic value: \(castedString)")
    }

    // Enums with associated values
    enum Result {
        case success(String)
        case failure(String)
    }
    let result: Result = .success("It worked!")
    switch result {
    case .success(let message):
        print("Result: \(message)")
    case .failure(let error):
        print("Error: \(error)")
    }

    // The joys of optionals and forced unwrapping
    var dict: [String: String]? = ["key": "value"]
    print("Dictionary value: \(dict!["key"]!)") // Forced unwrapping everywhere!

    // Memory management with ARC
    class MemoryLeak {
        var reference: MemoryLeak?
    }
    let instance1 = MemoryLeak()
    let instance2 = MemoryLeak()
    instance1.reference = instance2
    instance2.reference = instance1
    // These instances are now leaking memory!

    // Mutating method in struct
    struct MyStruct {
        var value = 0
        mutating func increment() {
            value += 1
        }
    }
    var myStruct = MyStruct()
    myStruct.increment()
    print("Mutating struct method: \(myStruct.value)")

    // Failable initializers
    struct FailableStruct {
        var value: Int
        init?(value: Int) {
            if value < 0 {
                return nil
            }
            self.value = value
        }
    }
    if let instance = FailableStruct(value: 10) {
        print("Failable initializer succeeded: \(instance.value)")
    }
    if let instance = FailableStruct(value: -1) {
        print("Failable initializer succeeded: \(instance.value)")
    } else {
        print("Failable initializer failed")
    }

    // Protocol extensions vs. class methods
    protocol MyProtocol {
        func myMethod()
    }

    extension MyProtocol {
        func myMethod() {
            print("Protocol extension method")
        }
    }

    class MyClass: MyProtocol {
        func myMethod() {
            print("Class method")
        }
    }

    let myInstance: MyProtocol = MyClass()
    myInstance.myMethod() // Calls the class method, not the protocol extension

    // Access control quirks
    class AccessControl {
        private var privateVar = "I'm private"
        fileprivate var fileprivateVar = "I'm fileprivate"
    }

    let accessControl = AccessControl()
    //print(accessControl.privateVar) // Error: 'privateVar' is inaccessible due to 'private' protection level
    print("Fileprivate variable: \(accessControl.fileprivateVar)") // Accessible within the same file

    // Threading issues
    var counter = 0
    let queue = DispatchQueue(label: "com.example.myqueue", attributes: .concurrent)
    let group = DispatchGroup()

    for _ in 0..<1000 {
        queue.async(group: group) {
            counter += 1 // Race condition!
        }
    }

    group.wait()
    print("Counter value after threading: \(counter)")

    // Swift's weird handling of character strings
    let weirdString = "cafe\u{301}" // café
    print("Weird string: \(weirdString)")

    // Enum raw values
    enum MyEnum: Int {
        case one = 1, two, three
    }
    let myEnum = MyEnum(rawValue: 2)
    print("Enum raw value: \(String(describing: myEnum))")

    // Using KVO (Key-Value Observing)
    class ObservableObject: NSObject {
        @objc dynamic var observedProperty: String = ""
    }

    let observableObject = ObservableObject()
    let observation = observableObject.observe(\.observedProperty, options: [.new]) { object, change in
        print("Observed property changed to: \(object.observedProperty)")
    }
    observableObject.observedProperty = "New value"

    // Custom operator overloads
    infix operator ** : MultiplicationPrecedence
    func ** (base: Int, power: Int) -> Int {
        return Int(pow(Double(base), Double(power)))
    }
    print("Custom operator overload 2 ** 3: \(2 ** 3)")

    print("End of wildly awesome function.")
}

swiftIsAwesome()
