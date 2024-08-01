<?php
function phpIsAwesome() {
    // Variable types are so flexible!
    $var = 42;
    echo "Initially an integer: $var\n";
    $var = "Now I'm a string!";
    echo "Now a string: $var\n";

    // Type juggling magic
    echo "Type juggling (string + number): " . ("5" + 10) . "\n"; // 15
    echo "Type juggling (string . number): " . ("5" . 10) . "\n"; // 510

    // Loose comparisons
    echo "Loose comparison (0 == '0'): " . (0 == '0' ? 'true' : 'false') . "\n"; // true
    echo "Loose comparison (0 == 'abc'): " . (0 == 'abc' ? 'true' : 'false') . "\n"; // true

    // Arrays are super versatile!
    $array = [1, 2, 3, 4, 5];
    $array['key'] = 'value';
    echo "Array with mixed keys: ";
    print_r($array);

    // Implicit global variables
    function createGlobalVariable() {
        global $globalVar;
        $globalVar = "I'm global!";
    }
    createGlobalVariable();
    echo "Accessing implicit global variable: $globalVar\n";

    // Function scope and variable scope
    function hoistingExample() {
        echo "Variable hoisting example: $hoistedVar\n"; // Notice: Undefined variable
        $hoistedVar = "I'm hoisted!";
        echo "After assignment: $hoistedVar\n";
    }
    hoistingExample();

    // Null and undefined handling
    $undefinedVar;
    $nullVar = null;
    echo "undefined == null: " . ($undefinedVar == $nullVar ? 'true' : 'false') . "\n"; // true
    echo "undefined === null: " . ($undefinedVar === $nullVar ? 'true' : 'false') . "\n"; // false

    // Array key quirks
    $array = [1, 2, 3];
    $array[] = 4;
    $array[10] = 5;
    $array[] = 6;
    echo "Array with non-continuous keys: ";
    print_r($array);

    // Inconsistent function naming
    echo "strlen('test'): " . strlen('test') . "\n";
    echo "str_replace('test', 'best', 'This is a test'): " . str_replace('test', 'best', 'This is a test') . "\n";
    echo "strpos('test', 'e'): " . strpos('test', 'e') . "\n";

    // Magic quotes (deprecated but still a legend)
    // if (get_magic_quotes_gpc()) {
    //     echo "Magic quotes are enabled!\n";
    // }

    // The great "goto" statement
    $count = 3;
    echo "Using goto statement:\n";
    COUNTDOWN:
    echo "$count\n";
    $count--;
    if ($count > 0) goto COUNTDOWN;

    // Implicit type conversion pitfalls
    $str = "10 apples";
    $num = 5;
    echo "Implicit type conversion (string + number): " . ($str + $num) . "\n"; // 15

    // PHP arrays are really hashmaps
    $array = [1, 2, 3];
    $array["key"] = "value";
    echo "Array treated as hashmap: ";
    print_r($array);

    // Case-insensitive function names
    echo "StRtOlOwEr('TEST'): " . StrToLower('TEST') . "\n";

    // Error handling with @ operator
    echo "Suppressing errors with @ operator: " . @file_get_contents('nonexistent_file.txt') . "\n"; // no warning

    echo "End of wildly awesome function.\n";
}

phpIsAwesome();
?>
