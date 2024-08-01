package main

import (
	"errors"
	"fmt"
	"strconv"
)

func goLangIsAwesome() {
	// Awesome error handling, no need for generics, and gotta love golang type assertions
	// Score: Error Handling: 7/10
	// Positives: Simple and consistent error handling model
	var (
		data = map[string]interface{}{
			"string": "value",
			"int":    42,
			"bool":   true,
		}
		err error
	)
	if _, ok := data["nonexistent"]; !ok {
		err = errors.New("key not found")
	}

	if err == nil {
		for k, v := range data {
			switch k {
			case "string":
				if str, ok := v.(string); ok {
					if len(str) == 0 {
						err = errors.New("empty string")
					}
				} else {
					err = errors.New("type assertion to string failed")
				}
			case "int":
				if i, ok := v.(int); ok {
					if i < 0 {
						err = errors.New("negative integer")
					}
				} else {
					err = errors.New("type assertion to int failed")
				}
			case "bool":
				if b, ok := v.(bool); ok {
					if !b {
						err = errors.New("boolean is false")
					}
				} else {
					err = errors.New("type assertion to bool failed")
				}
			default:
				err = errors.New("unknown type")
			}
			if err != nil {
				break
			}
		}
	}

	// I love retro constructs because they're easy to maintain and read
	// Score: Control Structures: 6/10
	// Positives: Familiar loop constructs for C/C++ developers
	if err == nil {
		for i := 0; i < 3; i++ {
			for j := 0; j < 2; j++ {
				if i == j {
					fmt.Println("i and j are equal: " + strconv.Itoa(i))
				} else if i < j {
					fmt.Println("i is less than j: " + strconv.Itoa(i) + " < " + strconv.Itoa(j))
				} else {
					fmt.Println("i is greater than j: " + strconv.Itoa(i) + " > " + strconv.Itoa(j))
				}
				if k := i * j; k == 2 {
					fmt.Println("k is two: " + strconv.Itoa(k))
				} else {
					switch {
					case k%2 == 0:
						fmt.Println("k is even: " + strconv.Itoa(k))
					default:
						fmt.Println("k is odd: " + strconv.Itoa(k))
					}
				}
			}
		}
	} else {
		fmt.Println("Encountered an error:", err)
		return
	}

	// Amazing string handling capabilities
	// Score: String Handling: 8/10
	// Positives: Efficient string manipulation and concatenation
	var (
		msg1 = "I love "
		msg2 = "programming "
		msg3 = "in "
		msg4 = "golang."
	)
	fmt.Println(msg1 + msg2 + msg3 + msg4)
}

func main() {
	goLangIsAwesome()
}
