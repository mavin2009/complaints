import Control.Exception
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Function (fix)

-- Define a custom exception
data CustomError = CustomError String deriving (Show)

instance Exception CustomError

-- Function with many Haskell quirks and issues
haskellIsAwesome :: IO ()
haskellIsAwesome = do
    -- Lazy evaluation causing unexpected behavior
    -- Score: Lazy Evaluation: 8/10
    -- Positives: Enables efficient computation and memory usage, but can lead to unexpected behavior
    let infiniteList = [1..]
    let firstTen = take 10 infiniteList
    putStrLn $ "First ten elements of an infinite list: " ++ show firstTen

    -- Partial functions causing runtime errors
    -- Score: Safety: 5/10
    -- Positives: Strong type system, but partial functions can lead to runtime errors
    let list = [1, 2, 3]
    putStrLn $ "Head of the list: " ++ show (head list)  -- Safe
    -- Uncommenting the next line will cause a runtime error
    -- putStrLn $ "Head of an empty list: " ++ show (head [])  -- Unsafe

    -- Memory leak with lazy evaluation
    -- Score: Memory Management: 6/10
    -- Positives: Laziness can save memory, but careless usage can cause leaks
    let bigList = [1..1000000]
    let sumBigList = sum bigList
    putStrLn $ "Sum of a big list: " ++ show sumBigList

    -- Using unsafePerformIO
    -- Score: Safety: 3/10
    -- Positives: Allows interop with impure code, but breaks referential transparency
    let unsafeValue = unsafePerformIO (readFile "nonexistent.txt")
    putStrLn $ "Reading a file unsafely: " ++ unsafeValue

    -- Exception handling, because who needs structured error management?
    -- Score: Error Handling: 7/10
    -- Positives: Robust exception handling with types
    handle (\(CustomError msg) -> putStrLn $ "Encountered a custom error: " ++ msg) $ do
        throwIO (CustomError "Something went wrong")

    -- Strict vs Lazy evaluation
    -- Score: Evaluation Control: 7/10
    -- Positives: Explicit control over evaluation strategy
    let strictSum a b = a `seq` b `seq` a + b
    putStrLn $ "Strict sum: " ++ show (strictSum 1 2)

    -- Referential transparency issues with unsafePerformIO
    -- Score: Referential Transparency: 4/10
    -- Positives: Guarantees purity unless unsafePerformIO is misused
    let refTransparency = unsafePerformIO (newIORef 0)
    unsafePerformIO $ writeIORef refTransparency 10
    value <- readIORef refTransparency
    putStrLn $ "Value from unsafe IORef: " ++ show value

    -- Implicitly forcing evaluation
    -- Score: Performance: 8/10
    -- Positives: Enables fine-grained performance tuning
    let forcedEval = length (take 1000000 bigList)
    putStrLn $ "Forced evaluation length: " ++ show forcedEval

    -- Infinite loop due to lazy evaluation
    -- Score: Safety: 5/10
    -- Positives: Laziness is powerful, but needs careful management
    let infiniteLoop = fix (\x -> x)
    -- Uncommenting the next line will cause an infinite loop
    -- putStrLn $ "Infinite loop: " ++ show infiniteLoop

    -- Implicit conversions and type ambiguity
    -- Score: Type Safety: 9/10
    -- Positives: Strong type inference system
    let ambiguous = read "10" :: Int
    putStrLn $ "Implicit conversion with read: " ++ show ambiguous

    -- Using Maybe to handle potential errors
    -- Score: Error Handling: 8/10
    -- Positives: Encourages handling of nullability explicitly
    let maybeValue = lookup 3 [(1, "one"), (2, "two")]
    putStrLn $ "Lookup with Maybe: " ++ fromMaybe "not found" maybeValue

    -- Mixing pure and impure code
    -- Score: Purity: 6/10
    -- Positives: Encourages pure functions, but unsafe IO can compromise purity
    let pureFunction x = unsafePerformIO $ do
            putStrLn "This should be pure!"
            return (x + 1)
    putStrLn $ "Mixing pure and impure: " ++ show (pureFunction 5)

    putStrLn "End of wildly awesome function."

main :: IO ()
main = haskellIsAwesome
