function javascriptIsAwesome() {
    // Dynamic typing at its best
    // Score: Type Safety: 3/10
    // Positives: Provides flexibility but can lead to runtime errors
    let variable = 42;
    console.log(`Initially a number: ${variable}`);
    variable = "Now I'm a string!";
    console.log(`Now a string: ${variable}`);

    // Type coercion fun
    // Score: Type Safety: 3/10
    // Positives: Allows for flexible operations but can be unpredictable
    console.log(`Type coercion (number + string): ${5 + "5"}`); // Concatenates to "55"
    console.log(`Type coercion (string - number): ${"5" - 3}`); // Subtracts to 2

    // Equality quirks
    // Score: Syntax Clarity: 4/10
    // Positives: Offers flexible comparisons but can lead to confusion
    console.log(`Equality with type coercion (==): ${0 == "0"}`); // true
    console.log(`Equality without type coercion (===): ${0 === "0"}`); // false

    // Floating point precision issues
    // Score: Numeric Precision: 5/10
    // Positives: Supports floating-point arithmetic but can be inaccurate
    console.log(`0.1 + 0.2 === 0.3: ${0.1 + 0.2 === 0.3}`); // false
    console.log(`0.1 + 0.2: ${0.1 + 0.2}`); // 0.30000000000000004

    // Array length manipulation
    // Score: Array Handling: 6/10
    // Positives: Dynamic arrays are flexible but can lead to unexpected results
    let arr = [1, 2, 3, 4, 5];
    arr.length = 3; // Truncates the array
    console.log(`Truncated array: ${arr}`);
    arr.length = 5; // Adds empty slots
    console.log(`Extended array: ${arr}`);

    // Implicit global variable
    // Score: Scoping Rules: 4/10
    // Positives: Flexible scoping but can lead to accidental global variables
    function createGlobalVariable() {
        globalVar = "I'm global!";
    }
    createGlobalVariable();
    console.log(`Accessing implicit global variable: ${globalVar}`);

    // Function scope and hoisting
    // Score: Scoping Rules: 5/10
    // Positives: Function hoisting offers flexibility but can be confusing
    console.log(`Function hoisting example: ${hoistedFunction()}`);
    function hoistedFunction() {
        return "Hoisted!";
    }

    // Hoisting with variables
    // Score: Scoping Rules: 4/10
    // Positives: Variable hoisting provides flexibility but can lead to undefined variables
    console.log(`Variable hoisting example: ${hoistedVar}`); // undefined
    var hoistedVar = "I'm hoisted!";
    console.log(`After assignment: ${hoistedVar}`);

    // Undefined and null
    // Score: Type Safety: 4/10
    // Positives: Distinct types but can be confusing
    let undefinedVar;
    let nullVar = null;
    console.log(`undefined == null: ${undefinedVar == nullVar}`); // true
    console.log(`undefined === null: ${undefinedVar === nullVar}`); // false

    // Closures and the infamous loop issue
    // Score: Syntax Clarity: 5/10
    // Positives: Closures are powerful but can lead to unexpected results in loops
    var funcs = [];
    for (var i = 0; i < 5; i++) {
        funcs.push(function () {
            console.log(`Closure with var, expected 0-4, got: ${i}`);
        });
    }
    funcs.forEach(function (func) {
        func();
    });

    // The fix with let
    // Score: Scoping Rules: 6/10
    // Positives: Let offers block scoping, resolving some closure issues
    funcs = [];
    for (let i = 0; i < 5; i++) {
        funcs.push(function () {
            console.log(`Closure with let, expected 0-4, got: ${i}`);
        });
    }
    funcs.forEach(function (func) {
        func();
    });

    // The infamous 'this' keyword
    // Score: Syntax Clarity: 4/10
    // Positives: Powerful but often leads to confusion
    const obj = {
        value: 42,
        getValue: function () {
            return this.value;
        }
    };
    console.log(`Correct 'this' context: ${obj.getValue()}`);
    const unboundGetValue = obj.getValue;
    console.log(`Incorrect 'this' context: ${unboundGetValue()}`);

    // Arrow functions to the rescue
    // Score: Syntax Clarity: 6/10
    // Positives: Arrow functions provide a predictable 'this' context
    const objArrow = {
        value: 42,
        getValue: () => objArrow.value
    };
    console.log(`Arrow function 'this' context: ${objArrow.getValue()}`);

    // NaN is a number
    // Score: Type Safety: 3/10
    // Positives: NaN indicates an invalid number but can be confusing
    console.log(`NaN is a number: ${typeof NaN}`);
    console.log(`NaN === NaN: ${NaN === NaN}`); // false
    console.log(`isNaN(NaN): ${isNaN(NaN)}`); // true

    // Deleting array elements
    // Score: Array Handling: 5/10
    // Positives: Provides flexibility but can lead to sparse arrays
    delete arr[1];
    console.log(`Array after delete: ${arr}`);
    console.log(`Array length after delete: ${arr.length}`);

    // Implicit type conversion with objects
    // Score: Type Safety: 4/10
    // Positives: Allows for flexible operations but can lead to unexpected results
    const objConversion = {
        toString: () => "42",
        valueOf: () => 3.14
    };
    console.log(`Implicit conversion (obj + string): ${objConversion + "!"}`); // "42!"
    console.log(`Implicit conversion (obj + number): ${objConversion + 1}`); // 4.14

    console.log("End of wildly awesome function.");
}

javascriptIsAwesome();
