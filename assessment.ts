class CustomError extends Error {
    constructor(message: string) {
        super(message);
        this.name = 'CustomError';
    }
}

async function typescriptIsAwesome() {
    // Implicit any and type inference issues
    let implicitAny; // No type specified, implicitly any
    implicitAny = 10;
    console.log(`Implicit any type: ${implicitAny}`);
    implicitAny = "Now I'm a string!";
    console.log(`Implicit any type changed: ${implicitAny}`);

    // Handling null and undefined
    let nullableValue: string | null = null;
    console.log(`Nullable value: ${nullableValue}`);
    nullableValue = "Now I'm not null!";
    console.log(`Nullable value: ${nullableValue}`);

    let undefinedValue: string | undefined;
    console.log(`Undefined value: ${undefinedValue}`);

    // Using any type
    let anyValue: any = 42;
    console.log(`Any type: ${anyValue}`);
    anyValue = "Now I'm a string!";
    console.log(`Any type changed: ${anyValue}`);

    // Array out of bounds
    let arr = [1, 2, 3];
    console.log(`Array element: ${arr[3]}`); // Undefined, no runtime error

    // Exception handling, because who needs structured error management?
    try {
        throw new CustomError("Something went wrong");
    } catch (e) {
        if (e instanceof CustomError) {
            console.log(`Encountered a custom error: ${e.message}`);
        }
    }

    // Async/await pitfalls
    async function asyncFunction() {
        return "Hello, async!";
    }

    async function withAwait() {
        console.log("Before await");
        const result = await asyncFunction();
        console.log(`Result: ${result}`);
        console.log("After await");
    }

    withAwait().catch(e => console.error(e));

    // This binding issues
    class MyClass {
        value = 42;

        getValue() {
            return this.value;
        }
    }

    const instance = new MyClass();
    const unboundGetValue = instance.getValue;
    try {
        console.log(`Unbound this value: ${unboundGetValue()}`); // Undefined or throws error
    } catch (e) {
        console.log("Caught unbound this error");
    }

    // Type assertion pitfalls
    let someValue: unknown = "This is a string";
    let strLength: number = (someValue as string).length;
    console.log(`String length: ${strLength}`);

    // Enum pitfalls
    enum Color {
        Red,
        Green,
        Blue
    }
    let color: Color = Color.Green;
    console.log(`Enum value: ${color}`);
    color = 3; // No error, but invalid enum value
    console.log(`Invalid enum value: ${color}`);

    // Implicit conversions and coercions
    let num: number = 10;
    let str: string = "5";
    let coercedResult = num + str;
    console.log(`Implicit conversion result: ${coercedResult}`); // Concatenates to "105"

    // TypeScript quirks with type guards
    function isString(value: any): value is string {
        return typeof value === 'string';
    }

    let unknownValue: unknown = "Hello, world!";
    if (isString(unknownValue)) {
        console.log(`String value: ${unknownValue}`);
    } else {
        console.log("Value is not a string");
    }

    // Type mismatch issues
    let numberArray: number[] = [1, 2, 3];
    let mixedArray: (number | string)[] = numberArray; // Allowed, but can cause issues
    mixedArray.push("A string!");
    console.log(`Mixed array: ${mixedArray}`);

    // Non-null assertion operator pitfalls
    let nullableStr: string | null = null;
    try {
        console.log(`Non-null asserted string length: ${nullableStr!.length}`); // Runtime error
    } catch (e) {
        console.log("Caught a runtime error due to non-null assertion operator on null");
    }

    // Complex type inference issues
    function complexTypeInference<T>(x: T) {
        return [x, x];
    }
    let inferredValue = complexTypeInference("hello");
    console.log(`Complex type inference: ${inferredValue}`);

    // Namespace and module pitfalls
    namespace MyNamespace {
        export class MyClass {
            greet() {
                return "Hello from namespace!";
            }
        }
    }

    const namespacedInstance = new MyNamespace.MyClass();
    console.log(namespacedInstance.greet());

    // Decorator usage pitfalls
    function logMethod(target: any, propertyKey: string, descriptor: PropertyDescriptor) {
        const originalMethod = descriptor.value;
        descriptor.value = function (...args: any[]) {
            console.log(`Calling ${propertyKey} with arguments: ${JSON.stringify(args)}`);
            return originalMethod.apply(this, args);
        };
        return descriptor;
    }

    class DecoratedClass {
        @logMethod
        decoratedMethod(arg: string) {
            return `Argument was: ${arg}`;
        }
    }

    const decoratedInstance = new DecoratedClass();
    console.log(decoratedInstance.decoratedMethod("test"));

    // Generic type pitfalls
    function genericFunction<T>(arg: T): T {
        return arg;
    }

    let genericValue = genericFunction<string>("Hello, generics!");
    console.log(`Generic function: ${genericValue}`);

    // Mixins and multiple inheritance issues
    function applyMixins(derivedCtor: any, baseCtors: any[]) {
        baseCtors.forEach(baseCtor => {
            Object.getOwnPropertyNames(baseCtor.prototype).forEach(name => {
                Object.defineProperty(
                    derivedCtor.prototype,
                    name,
                    Object.getOwnPropertyDescriptor(baseCtor.prototype, name) || Object.create(null)
                );
            });
        });
    }

    class CanEat {
        eat() {
            console.log("Eating...");
        }
    }

    class CanSleep {
        sleep() {
            console.log("Sleeping...");
        }
    }

    class Human implements CanEat, CanSleep {
        eat!: () => void;
        sleep!: () => void;
    }

    applyMixins(Human, [CanEat, CanSleep]);
    const human = new Human();
    human.eat();
    human.sleep();

    // Advanced async/await issues
    async function advancedAsyncFunction() {
        return new Promise((resolve, reject) => {
            setTimeout(() => resolve("Async result"), 1000);
        });
    }

    async function runAdvancedAsync() {
        console.log("Before advanced await");
        const result = await advancedAsyncFunction();
        console.log(`Advanced async result: ${result}`);
        console.log("After advanced await");
    }

    runAdvancedAsync().catch(e => console.error(e));

    // Conditional types pitfalls
    type MessageOf<T> = T extends { message: unknown } ? T["message"] : never;
    type ErrorMessage = MessageOf<CustomError>; // Expected string

    console.log("Conditional types: ", typeof ErrorMessage);

    // Recursive type definitions pitfalls
    type NestedNumbers = number | NestedNumbers[];
    let nestedNumber: NestedNumbers = [1, [2, [3, 4]], 5];
    console.log(`Recursive type definition: ${nestedNumber}`);

    // Excessive type assertions
    let excessiveAssertions: any = "Hello, world!";
    console.log(`Excessive assertions: ${((excessiveAssertions as number) as boolean) as string}`);

    // Type narrowing pitfalls
    function typeNarrowing(value: string | number) {
        if (typeof value === 'string') {
            console.log(`Type narrowed to string: ${value}`);
        } else if (typeof value === 'number') {
            console.log(`Type narrowed to number: ${value}`);
        } else {
            console.log(`Type not narrowed properly: ${value}`);
        }
    }

    typeNarrowing(10);
    typeNarrowing("Hello");
    typeNarrowing(true as any); // Improper type narrowing

    // Module resolution issues
    // import { SomeModule } from 'some-nonexistent-module'; // This will cause module resolution error

    // Exhaustiveness checking issues
    type Fruit = 'Apple' | 'Banana' | 'Orange';
    function getFruitName(fruit: Fruit): string {
        switch (fruit) {
            case 'Apple':
                return 'Apple';
            case 'Banana':
                return 'Banana';
            default:
                const exhaustiveCheck: never = fruit; // Should catch exhaustiveness issues
                return exhaustiveCheck; // This will cause an error if not exhaustive
        }
    }

    console.log(`Exhaustiveness checking: ${getFruitName('Apple')}`);

    console.log("End of wildly awesome function.");
}

typescriptIsAwesome().catch(e => console.error(`Unhandled error: ${e.message}`));
