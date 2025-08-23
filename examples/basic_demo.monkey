// Basic Monkey language demonstration

// Variables and arithmetic
let x = 5;
let y = 10;
let result = x + y * 2;

// Functions
let add = fn(a, b) { a + b };
let multiply = fn(a, b) { a * b };

// Function calls
let sum = add(x, y);
let product = multiply(x, y);

// Arrays
let numbers = [1, 2, 3, 4, 5];
let first = numbers[0];

// Hash maps  
let person = {"name": "Alice", "age": 30};
let name = person["name"];

// Conditionals
let max = fn(a, b) {
    if (a > b) {
        a
    } else {
        b
    }
};

let maximum = max(x, y);

// Return the final result
maximum