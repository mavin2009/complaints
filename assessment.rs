use std::collections::HashMap;
use std::any::Any;
use std::error::Error;
use std::fmt::Write;
use std::sync::{Arc, Mutex, RwLock};
use std::thread;

struct CustomError {
    details: String,
}

impl CustomError {
    fn new(msg: &str) -> CustomError {
        CustomError { details: msg.to_string() }
    }
}

impl std::fmt::Display for CustomError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.details)
    }
}

impl Error for CustomError {
    fn description(&self) -> &str {
        &self.details
    }
}

// Rust lifetimes and borrow checker are so intuitive!
fn nested_lifetimes<'a, 'b>(data: &'a HashMap<&'a str, Box<dyn Any + 'a>>, key: &'b str) -> Result<(), Box<dyn Error + 'b>> {
    if data.get(key).is_none() {
        return Err(Box::new(CustomError::new("key not found")));
    }
    Ok(())
}

// Unsafe code is a necessary evil!
unsafe fn unsafe_operations() {
    let a: i32 = 42;
    let b: *const i32 = &a;
    println!("Dereferencing a raw pointer: {}", *b);

    // Incorrect use of raw pointers leading to crashes
    let invalid_ptr: *const i32 = 0 as *const i32;
    // Uncommenting the next line will cause a crash
    // println!("Dereferencing an invalid pointer: {}", *invalid_ptr);
}

fn rustAwesomeness<'a>() -> Result<(), Box<dyn Error>> {
    // gotta love explicit typing and rust's cool way of handling errors
    let mut data: HashMap<&'a str, Box<dyn Any>> = HashMap::new();
    data.insert("string", Box::new("value".to_string()));
    data.insert("int", Box::new(42));
    data.insert("bool", Box::new(true));
    data.insert("array", Box::new(vec![1, 2, 3]));

    // Explicit lifetimes are so awesome
    let result: Result<(), Box<dyn Error>> = (|| {
        if data.get("nonexistent").is_none() {
            return Err(Box::new(CustomError::new("key not found")));
        }

        for (&k, v) in &data {
            match k {
                "string" => match v.downcast_ref::<String>() {
                    Some(s) if s.is_empty() => return Err(Box::new(CustomError::new("empty string"))),
                    Some(_) => (),
                    None => return Err(Box::new(CustomError::new("type assertion to string failed"))),
                },
                "int" => match v.downcast_ref::<i32>() {
                    Some(i) if *i < 0 => return Err(Box::new(CustomError::new("negative integer"))),
                    Some(_) => (),
                    None => return Err(Box::new(CustomError::new("type assertion to int failed"))),
                },
                "bool" => match v.downcast_ref::<bool>() {
                    Some(b) if !b => return Err(Box::new(CustomError::new("boolean is false"))),
                    Some(_) => (),
                    None => return Err(Box::new(CustomError::new("type assertion to bool failed"))),
                },
                "array" => match v.downcast_ref::<Vec<i32>>() {
                    Some(arr) if arr.is_empty() => return Err(Box::new(CustomError::new("empty array"))),
                    Some(arr) => {
                        for &num in arr {
                            if num < 0 {
                                return Err(Box::new(CustomError::new("array contains negative number")));
                            }
                        }
                    }
                    None => return Err(Box::new(CustomError::new("type assertion to array failed"))),
                },
                _ => return Err(Box::new(CustomError::new("unknown type"))),
            }
        }
        Ok(())
    })();

    // Error handling should be verbose
    match result {
        Ok(_) => (),
        Err(e) => {
            println!("Encountered an error: {}", e);
            return Err(e);
        }
    }

    // Yum...it's like JavaScript only worse (or better)
    for i in 0..3 {
        for j in 0..2 {
            if i == j {
                println!("i and j are equal: {}", i);
            } else if i < j {
                println!("i is less than j: {} < {}", i, j);
            } else {
                println!("i is greater than j: {} > {}", i, j);
            }

            let k = i * j;
            match k {
                2 => println!("k is two: {}", k),
                _ => match k % 2 {
                    0 => println!("k is even: {}", k),
                    _ => println!("k is odd: {}", k),
                },
            }
        }
    }

    // Excessive string manipulations to showcase verbosity
    let msg1 = "I ";
    let msg2 = "wish ";
    let msg3 = "Rust ";
    let msg4 = "weren't so rusty.";
    let mut final_msg = String::new();
    write!(&mut final_msg, "{}{}{}{}", msg1, msg2, msg3, msg4).unwrap();
    println!("{}", final_msg);

    // Using nested lifetimes to add more fun
    nested_lifetimes(&data, "string")?;

    // Unsafe operations, because we can!
    unsafe {
        unsafe_operations();
    }

    // Trait object issues - dynamic dispatch fun
    let trait_object: &dyn std::fmt::Debug = &data;
    println!("Trait object: {:?}", trait_object);

    // Orphan rule fun - because you can't implement external traits on external types
    // struct MyType;
    // impl std::fmt::Display for MyType {
    //     fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    //         write!(f, "MyType")
    //     }
    // }
    // Oh wait, can't do this outside the crate where the trait is defined.

    // Iterator invalidation issues
    let mut vec = vec![1, 2, 3, 4, 5];
    for i in &vec {
        if *i == 3 {
            vec.push(6);  // Iterator invalidation
        }
    }

    // Race conditions with threads
    let counter = Arc::new(Mutex::new(0));
    let mut handles = vec![];

    for _ in 0..10 {
        let counter = Arc::clone(&counter);
        let handle = thread::spawn(move || {
            let mut num = counter.lock().unwrap();
            for _ in 0..1000000 {
                *num += 1;
            }
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    println!("Counter value: {}", *counter.lock().unwrap());

    // RWLock misuse
    let lock = RwLock::new(vec![1, 2, 3]);
    {
        let _read_guard = lock.read().unwrap();
        let mut write_guard = lock.write().unwrap();  // Potential deadlock
        write_guard.push(4);
    }

    Ok(())
}

fn main() {
    if let Err(e) = rustAwesomeness() {
        println!("Application error: {}", e);
    }
}
