// While loop examples for Monkey language

puts("=== Basic While Loop ===");
let i = 0;
while (i < 5) {
    puts("Count: " + string(i));
    i = i + 1;
}

puts("\n=== While Loop with Accumulator ===");
let sum = 0;
let j = 1;
while (j <= 10) {
    sum = sum + j;
    j = j + 1;
}
puts("Sum of 1 to 10: " + string(sum));

puts("\n=== Fibonacci with While Loop ===");
let fibonacci = fn(n) {
    if (n <= 1) {
        return n;
    }
    
    let a = 0;
    let b = 1;
    let count = 2;
    
    while (count <= n) {
        let temp = a + b;
        a = b;
        b = temp;
        count = count + 1;
    }
    
    return b;
};

let fib_num = 8;
puts("Fibonacci(" + string(fib_num) + ") = " + string(fibonacci(fib_num)));

puts("\n=== Factorial with While Loop ===");
let factorial = fn(n) {
    let result = 1;
    let i = 1;
    while (i <= n) {
        result = result * i;
        i = i + 1;
    }
    return result;
};

let fact_num = 5;
puts(string(fact_num) + "! = " + string(factorial(fact_num)));

puts("\n=== Conditional While Loop ===");
let countdown = 10;
while (countdown > 0) {
    if (countdown == 5) {
        puts("Halfway there!");
    }
    puts("T-minus " + string(countdown));
    countdown = countdown - 1;
}
puts("Blastoff! 🚀");