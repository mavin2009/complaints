import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class complaints {

  public static void main(String[] args) {
    // Null handling issues
    // Score: Null Safety: 4/10
    // Positives: Java introduced Optional in newer versions to handle nullability
    String nullString = null;
    try {
      System.out.println(nullString.length());
    } catch (NullPointerException e) {
      System.out.println("Caught NullPointerException: " + e.getMessage());
    }

    // Type erasure issues with generics
    // Score: Generics: 6/10
    // Positives: Generics improve type safety but type erasure can lead to runtime issues
    List<String> stringList = new ArrayList<>();
    stringList.add("Hello");
    // This causes a compile-time warning
    List rawList = stringList;
    rawList.add(123); // No error at compile time
    try {
      String s = stringList.get(1); // ClassCastException at runtime
    } catch (ClassCastException e) {
      System.out.println("Caught ClassCastException: " + e.getMessage());
    }

    // Checked exceptions verbosity
    // Score: Error Handling: 7/10
    // Positives: Forces handling of exceptions but can be verbose
    try {
      readFile("nonexistent_file.txt");
    } catch (IOException e) {
      System.out.println("Caught IOException: " + e.getMessage());
    }

    // Synchronization problems
    // Score: Concurrency: 7/10
    // Positives: Java provides synchronized blocks and concurrent utilities but can be complex
    Counter counter = new Counter();
    Thread t1 = new Thread(() -> incrementCounter(counter));
    Thread t2 = new Thread(() -> incrementCounter(counter));
    t1.start();
    t2.start();
    try {
      t1.join();
      t2.join();
    } catch (InterruptedException e) {
      System.out.println("Caught InterruptedException: " + e.getMessage());
    }
    System.out.println("Counter value: " + counter.getValue());

    // Verbose syntax
    // Score: Syntax: 6/10
    // Positives: Verbose but promotes readability and explicitness
    Map<String, Integer> map = new HashMap<>();
    map.put("key", 123);
    if (map.containsKey("key")) {
      int value = map.get("key");
      System.out.println("Map value: " + value);
    }

    // Memory leaks due to improper resource handling
    // Score: Memory Management: 6/10
    // Positives: Automatic garbage collection but requires manual resource management
    try {
      File file = new File("nonexistent_file.txt");
      java.io.FileInputStream fis = new java.io.FileInputStream(file);
    } catch (FileNotFoundException e) {
      System.out.println("Caught FileNotFoundException: " + e.getMessage());
    }

    // Enum pitfalls
    // Score: Enums: 8/10
    // Positives: Strongly typed enums, but limited in extensibility
    Direction direction = Direction.NORTH;
    switch (direction) {
      case NORTH:
        System.out.println("Going north");
        break;
      case SOUTH:
        System.out.println("Going south");
        break;
      case EAST:
        System.out.println("Going east");
        break;
      case WEST:
        System.out.println("Going west");
        break;
      default:
        System.out.println("Unknown direction");
        break;
    }

    // Type inference issues with var
    // Score: Type Inference: 7/10
    // Positives: Reduces verbosity but less explicit
    var inferredInt = 10; // inferred as int
    var inferredString = "hello"; // inferred as String
    System.out.println("Inferred int: " + inferredInt);
    System.out.println("Inferred string: " + inferredString);

    // Implicit conversion pitfalls
    // Score: Type Safety: 8/10
    // Positives: Strongly typed but implicit conversions can lead to errors
    int num = 10;
    String str = "5";
    String result = num + str; // Implicitly converts num to String
    System.out.println("Implicit conversion result: " + result);

    // Classpath issues and dynamic class loading
    // Score: Flexibility: 7/10
    // Positives: Dynamic class loading enables flexible applications but can cause runtime issues
    try {
      Class<?> cls = Class.forName("java.util.Random");
      System.out.println("Loaded class: " + cls.getName());
    } catch (ClassNotFoundException e) {
      System.out.println("Caught ClassNotFoundException: " + e.getMessage());
    }

    // Serialization issues
    // Score: Serialization: 6/10
    // Positives: Built-in serialization, but requires careful handling of serialVersionUID
    SerializableClass serializableClass = new SerializableClass();
    try {
      java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
      java.io.ObjectOutputStream oos = new java.io.ObjectOutputStream(baos);
      oos.writeObject(serializableClass);
      oos.close();
      java.io.ByteArrayInputStream bais = new java.io.ByteArrayInputStream(
        baos.toByteArray()
      );
      java.io.ObjectInputStream ois = new java.io.ObjectInputStream(bais);
      SerializableClass deserializedClass = (SerializableClass) ois.readObject();
      ois.close();
      System.out.println("Deserialized object: " + deserializedClass);
    } catch (IOException | ClassNotFoundException e) {
      System.out.println(
        "Caught exception during serialization: " + e.getMessage()
      );
    }

    // Class/interface inheritance/casting issues
    // Score: Inheritance: 7/10
    // Positives: Powerful OOP model, but can lead to complex hierarchies
    Base base = new Derived();
    if (base instanceof Derived) {
      Derived derived = (Derived) base;
      derived.showValue();
    }

    // Autoboxing pitfalls
    // Score: Performance: 6/10
    // Positives: Convenience but can lead to performance issues
    Integer a = 1000;
    Integer b = 1000;
    System.out.println("Autoboxing comparison: " + (a == b)); // false due to different object references
    Integer c = 100;
    Integer d = 100;
    System.out.println("Autoboxing comparison: " + (c == d)); // true due to caching of small integers

    // Memory leaks with static fields
    // Score: Memory Management: 6/10
    // Positives: Static fields are powerful but can cause memory leaks if not managed
    LeakyClass leaky = new LeakyClass();
    leaky.leak();

    // Garbage collection issues
    // Score: Garbage Collection: 8/10
    // Positives: Automatic memory management, but with potential overhead
    Runtime runtime = Runtime.getRuntime();
    System.out.println("Total memory: " + runtime.totalMemory());
    System.out.println("Free memory: " + runtime.freeMemory());
    for (int i = 0; i < 1000000; i++) {
      new GarbageObject();
    }
    System.gc();
    System.out.println("Free memory after GC: " + runtime.freeMemory());

    // Overloading/overriding issues
    // Score: Polymorphism: 8/10
    // Positives: Allows for polymorphic behavior but can lead to confusion
    OverloadingAndOverriding example = new OverloadingAndOverriding();
    example.doSomething("string");
    example.doSomething(123);

    System.out.println("End of wildly awesome function.");
  }

  public static void readFile(String filename) throws IOException {
    throw new IOException("File not found");
  }

  public static void incrementCounter(Counter counter) {
    for (int i = 0; i < 1000; i++) {
      counter.increment();
    }
  }

  static class Counter {

    private int value = 0;

    public synchronized void increment() {
      value++;
    }

    public int getValue() {
      return value;
    }
  }

  enum Direction {
    NORTH,
    SOUTH,
    EAST,
    WEST,
  }

  static class SerializableClass implements java.io.Serializable {

    private static final long serialVersionUID = 1L;
  }

  static class Base {

    public void showValue() {
      System.out.println("Base class");
    }
  }

  static class Derived extends Base {

    @Override
    public void showValue() {
      System.out.println("Derived class");
    }
  }

  static class LeakyClass {

    private static List<Object> leakyList = new ArrayList<>();

    public void leak() {
      for (int i = 0; i < 1000; i++) {
        leakyList.add(new Object());
      }
    }
  }

  static class GarbageObject {

    private byte[] memory = new byte[1024];
  }

  static class OverloadingAndOverriding {

    public void doSomething(String str) {
      System.out.println("Doing something with string: " + str);
    }

    public void doSomething(int num) {
      System.out.println("Doing something with int: " + num);
    }
  }
}
