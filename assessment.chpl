// assessment.chpl: Chapel is versatile and performant!

use IO;
use Time;
use SysCTypes;

class CustomError : Error {
  var message: string;
  proc init(msg: string) {
    this.message = msg;
    super.init();
  }
  proc message() const override {
    return this.message;
  }
}

proc chapelIsAwesome() {
  // Implicit conversions and type ambiguity
  // Score: Type Safety: 7/10
  // Positives: Strongly typed, compile-time errors prevent unexpected behavior
  var ambiguous: int = "10"; // This will cause a compile-time error
  writeln("Implicit conversion with assignment: ", ambiguous);

  // Array bounds issues
  // Score: Error Handling: 6/10
  // Positives: Supports compile-time checks to prevent out-of-bounds access
  var arr = [1, 2, 3];
  writeln("Array element: ", arr(4)); // Access out of bounds, causes runtime error

  // Using uninitialized variables
  // Score: Type Safety: 7/10
  // Positives: Allows developers to spot undefined behavior during development
  var uninitializedVar: int;
  writeln("Uninitialized variable value: ", uninitializedVar); // Undefined behavior

  // Memory leak with unmanaged classes
  // Score: Memory Management: 6/10
  // Positives: Supports managed classes and garbage collection to handle memory efficiently
  class LeakyClass {
    var value: int;
    proc init(v: int) {
      this.value = v;
    }
  }
  var leakyInstance: unmanaged LeakyClass = new unmanaged LeakyClass(42);
  // No delete, causes memory leak

  // Exception handling, because who needs structured error management?
  // Score: Error Handling: 6/10
  // Positives: Supports structured exception handling for robust applications
  try {
    throw new owned CustomError("Something went wrong");
  } catch e: CustomError {
    writeln("Encountered a custom error: ", e.message());
  }

  // Parallelism issues with data races
  // Score: Concurrency Support: 8/10
  // Positives: Provides built-in parallel constructs for efficient concurrent programming
  var counter = 0;
  cobegin {
    for 1..1000000 do
      counter += 1;

    for 1..1000000 do
      counter -= 1;
  }
  writeln("Counter value with data races: ", counter); // Should be 0 but might not be

  // Synchronization issues
  // Score: Concurrency Support: 8/10
  // Positives: Offers sync and atomic variables for proper synchronization
  var syncCounter: sync int = 0;
  cobegin {
    syncCounter += 1;
    syncCounter += 1;
  }
  writeln("Sync counter value: ", syncCounter); // Might be incorrect

  // Race conditions with parallelism
  // Score: Concurrency Support: 8/10
  // Positives: Provides easy-to-use parallel constructs to avoid race conditions
  var raceCounter = 0;
  cobegin {
    for 1..1000000 do
      raceCounter += 1;

    for 1..1000000 do
      raceCounter += 1;
  }
  writeln("Race condition counter value: ", raceCounter); // Should be 2000000 but might not be

  // Using deprecated features
  // Score: Developer Ergonomics: 7/10
  // Positives: Encourages best practices by discouraging deprecated features
  var deprecatedVar: int = 123;
  writeln("Deprecated variable: ", deprecatedVar);

  // Using unsafe code
  // Score: Type Safety: 7/10
  // Positives: Allows explicit control over memory for performance-critical code
  proc unsafeCode() {
    var a: int = 42;
    var b: c_ptr(int) = c_ptr(int)(&a);
    writeln("Dereferencing a raw pointer: ", b: int);
  }
  unsafeCode();

  // Mixing pure and impure code
  // Score: Developer Ergonomics: 7/10
  // Positives: Supports functional programming with pure functions
  proc pureFunction(x: int): int {
    writeln("This should be pure!");
    return x + 1;
  }
  writeln("Mixing pure and impure: ", pureFunction(5));

  // Using external libraries unsafely
  // Score: Cross-Platform Support: 7/10
  // Positives: Offers interoperability with C and other languages
  extern proc printf(format: c_string, ...): int;
  printf("Using external libraries unsafely: %s\n".c_string, "Hello, Chapel!".c_string);

  // Division by zero
  // Score: Error Handling: 6/10
  // Positives: Provides mechanisms to catch and handle runtime errors gracefully
  var a = 10;
  var b = 0;
  try {
    writeln("Division by zero: ", a / b); // Will cause a runtime error
  } catch e: SysError {
    writeln("Caught division by zero error: ", e.message());
  }

  // File handling issues
  // Score: Cross-Platform Support: 7/10
  // Positives: Provides straightforward file I/O operations
  var file: file;
  try {
    file = open("nonexistent_file.txt", iomode.c_r);
    writeln("File content: ", read(file));
  } catch e: SysError {
    writeln("Caught file handling error: ", e.message());
  } finally {
    if file != nil then
      close(file);
  }

  // Improper use of atomic variables
  // Score: Concurrency Support: 8/10
  // Positives: Offers atomic operations for thread-safe programming
  var atomicCounter: atomic int;
  atomicCounter.write(0);
  cobegin {
    for 1..1000000 do
      atomicCounter.add(1);

    for 1..1000000 do
      atomicCounter.add(1);
  }
  writeln("Improper atomic counter value: ", atomicCounter.read()); // Should be 2000000 but might not be

  // Use of undefined behavior with nil
  // Score: Type Safety: 7/10
  // Positives: Encourages safe programming practices by handling null pointers
  var nilPtr: c_ptr(int) = nil;
  writeln("Dereferencing nil pointer: ", nilPtr: int); // Undefined behavior

  writeln("End of wildly awesome function.");
}

proc main() {
  chapelIsAwesome();
}
