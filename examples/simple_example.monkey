// Simple Monkey language example

// Basic arithmetic
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
let last = numbers[4];

// Hash maps
let person = {"name": "Alice", "age": 30};
let name = person["name"];
let age = person["age"];

// Print some results
puts("x + y * 2 = " + string(result));
puts("add(x, y) = " + string(sum));
puts("multiply(x, y) = " + string(product));
puts("first number = " + string(first));
puts("last number = " + string(last));
puts("person name = " + name);
puts("person age = " + string(age));