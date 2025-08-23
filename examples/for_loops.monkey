// For loop examples for Monkey language

puts("=== Basic For Loop with Array ===");
for (x in [1, 2, 3, 4, 5]) {
    puts("Element: " + string(x));
}

puts("\n=== For Loop with Accumulator ===");
let sum = 0;
for (x in [1, 2, 3, 4, 5]) {
    sum = sum + x;
}
puts("Sum of 1 to 5: " + string(sum));

puts("\n=== For Loop with Hash Keys ===");
let person = {"name": "John", "age": 30, "city": "New York"};
puts("Person properties:");
for (key in person) {
    puts("- " + string(key));
}

puts("\n=== Array Transformation ===");
let numbers = [1, 2, 3, 4];
let doubled = [];
for (num in numbers) {
    doubled = push(doubled, num * 2);
}
puts("Original: " + string(numbers));
puts("Doubled:  " + string(doubled));

puts("\n=== Variable Scoping Test ===");
let x = 100;
puts("Before loop: x = " + string(x));
for (x in [1, 2, 3]) {
    puts("Inside loop: x = " + string(x));
}
puts("After loop: x = " + string(x)); // Should still be 100

puts("\n=== Nested For Loops ===");
let matrix = [[1, 2], [3, 4]];
puts("Matrix elements:");
for (row in matrix) {
    for (col in row) {
        puts("  " + string(col));
    }
}

puts("\n=== Early Return from For Loop ===");
let find_target = fn(arr, target) {
    for (x in arr) {
        if (x == target) {
            return "Found " + string(target) + "!";
        }
    }
    return "Not found";
};
puts(find_target([10, 20, 30, 40], 30));
puts(find_target([10, 20, 30, 40], 50));

puts("\n=== Empty Collections ===");
puts("Empty array:");
for (x in []) {
    puts("This should not print");
}
puts("Empty hash:");
for (x in {}) {
    puts("This should not print");
}
puts("Done with empty collections");