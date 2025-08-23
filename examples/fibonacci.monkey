// Fibonacci calculation example
let fibonacci = fn(x) {
    if (x < 2) {
        return x;
    } else {
        return fibonacci(x - 1) + fibonacci(x - 2);
    }
};

// Helper function to print fibonacci numbers for an array
let printFibonacci = fn(numbers, index) {
    if (index >= len(numbers)) {
        return null;
    }
    
    let num = numbers[index];
    let result = fibonacci(num);
    puts("fibonacci(" + string(num) + ") = " + string(result));
    
    // Recursive call to process next number
    printFibonacci(numbers, index + 1);
};

// Calculate fibonacci for several numbers
let numbers = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
printFibonacci(numbers, 0);