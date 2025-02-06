# pipsqueak
*/pĭp′skwēk″/*
1. *One that is small or insignificant*
---
Pipsqueak is planned to be an interpreter for a variant of the
[monkey programming language](https://monkeylang.org/). The project primarily serves as a subject of exploratory 
learning but might in the future be extended for use in other projects where a custom programming language could come 
in handy. 

Some liberties have been taken with the design of the language, and I hope to further experiment with implementing new 
fun little features when the mood strikes.

## Language
### Keywords
The following is a list of all reserved keywords in the Pipsqueak language. Attempting to use any of these words for 
anything but its intended purpose result in a parsing or evaluation error.

#### let 
The `let` keywords allows the user to store an expression as identified by a name. In other words, `let` is used to 
declare a variable. The following is an example of using it.
```
let five = 5;
let ten = 10;
let fifteen = five + ten;
```
The expressions in the example above are simple literals and arithmetic expressions. Within Pipsqueak, you also use the 
`let` keyword to name functions and imported files. An example of this is shown below.
```
let add = fn(a, b) {
    return a + b;
};

let math = import "math.sqk";
print(math.sqrt(4));
```
The example above introduces two new keywords, namely `fn` and `import`. Let's take a closer look at them.

#### fn
The `fn` keyword is used to define a function. The `fn` keyword is expected to be followed by a parameter list -which 
can be empty- and then a function body containing the statements to be executed once the function is called. 
```
let add = fn(a, b) {
    return a + b;
};
print(add(5, 10));

let sub = fn(a, b) {
    return a - b;
};
print(sub(10, 5));
```

#### return
This keyword is used to send a value back to the caller of a function. The type of the returned value can be anything 
(or nothing!). The code examples for the `fn` keyword already provides examples the usage of `return`. One other useful 
attribute of the `return` keyword is that it breaks the control flow from the point where it is evaluated, this 
attribute is why "returning nothing" can be of great use. An example of this is given below.
```
let fib = fn(n) {
    if (n < 2) {
        return n;
    }
    return fib(n-1) + fib(n-2);
}
```
The return keyword is not required if you are returning the evaluated value of the final statement in the function body,
this value is automatically returned. Hence, the above example can be rewritten as any of the two below.
```
let fib = fn(n) {
    if (n < 2) {
        return n;
    }
    fib(n-1) + fib(n-2);
}
```
```
let fib = fn(n) {
    if (n < 2) {
        n;
    } else {
        fib(n-1) + fib(n-2);
    }
}
```
Which one you would choose comes down to the situation you're in and personal preference.

#### import
The `import` keyword reads and executes another script file in an isolated scope and returns the evaluator that was used 
to run the script. This allows a user to access code written in other files by referencing the script by its identifier 
and using full-stop operators further reference the identifier within the other script file. The following is an example 
using the `import` keyword.
```
import "header_printer.sqk";

let math = import "math.sqk";
math.sqrt(4);
```

#### if 
The `if` and `else` keywords are tightly related and helps facilitate different branching of execution flow. The `if` 
keyword expects an expression wrapped within parenthesis to follow it. This expression must evaluate to a boolean value. 
If the resulting value is `true` then the statement following the `if` condition is executed. Otherwise, if an `else` 
block is defined then the code within it is evaluated. The following code show some trivial examples of using `if` and 
`else`.
```
if (1 == 1) {
    print("This is executed");
} else {
    print("This is not executed");
}

if (1 == 2) {
    print("This is not executed");
} else {
    print("This is executed");
}

if (2 == 2) {
    print("This is executed");
}
```

#### while 
The final keyword, `while` is similar to `if` except it will continue to evaluate the statement following the `while` 
declaration until the expression within the parenthesis evaluates to `false`. The following code shows a simple `while` 
loop.
```
let i = 0;
while (i < 10) {
    print(i);
}
```