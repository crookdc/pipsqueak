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

## Examples

### Fibonacci
```
let fib = fn(n) {
  if (n < 2) {
    return n;
  }
  fib(n-1) + fib(n-2)
};

print(fib(7));
```

### Printing elements of a list
```
let names = ["Crookd", "Brookd", "Lookd"];
let i = 0;
while (i < len(names)) {
  print(names[i], " is really cool!");
  i = i + 1;
}
```
