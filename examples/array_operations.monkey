// Array operations example
let arr = [1, 2, 3, 4, 5];

// Map function
let map = fn(arr, f) {
    let result = [];
    let i = 0;
    while (i < len(arr)) {
        result = push(result, f(arr[i]));
        i = i + 1;
    }
    result
};

// Double each element
let double = fn(x) { x * 2 };
let doubled = map(arr, double);

puts("Original: " + string(arr));
puts("Doubled: " + string(doubled));

// Sum all elements
let sum = fn(arr) {
    let total = 0;
    let i = 0;
    while (i < len(arr)) {
        total = total + arr[i];
        i = i + 1;
    }
    total
};

puts("Sum of original: " + string(sum(arr)));
// puts("Sum of doubled: " + string(sum(doubled)));