# Advanced Programming: REPL Calculators

In this exercise you will be making two calculator [REPLs](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop). The first of these is a regular calculator dealing with integers, the second operates on [MultiSets](#multisets). A lot of the techniques discussed in this course are useful to enable code reuse and this assignment gives you ample opportunity to employ these techniques since both REPLs have very similar functionality.
 
The REPLs have three types of command:

- **Expression Evaluation:** If the input is an expression (for example: `1 + n * 3 + 5`) then the output should be its result (18 in this example, assuming n = 4)
- **Variable Assignment:** If the input is an assignment (for example: `n = 18 * m * 2 + 5)`, the REPL should store the binding of that variable and print the new binding (in this case: `n = 77`, assuming m = 2).
- **Expression Simplification:** If the input starts with an "@" then the REPL should simplify the expression after the "@" according to the [rules below](#simplification-rules). For example, `@ (n * 0) + (n * 2) + (n * 3) + a * b` should give `n * (2 + 3) + a * b`

## MultiSets
A multiset is like a set, but allows for duplicates. The number of instances of an element in a multiset is called its *multiplicity*. For example, in the multiset `{ a, a, a, b, a }`, `a` has a multiplicity of 4, and `b` has a multiplicity of 1. The multiplicity is always an integer and never negative. The multiplicity of an element which is not in the multiset is 0.

You will implement three multiset operations:

- **Summation:** a + b is the multiset c such that:
    
    <code>∀x m<sub>c</sub>(x) = m<sub>a</sub>(x) + m<sub>b</sub>(x)</code>

    where <code>m<sub>c</sub>(x)</code> is the multiplicity of x in the set c.

- **Intersection:** a * b is the multiset c such that:

    <code>∀x m<sub>c</sub>(x) = min(m<sub>a</sub>(x), m<sub>b</sub>(x))</code>

- **Subtraction:** a - b is the multiset c such that:

    <code>∀x m<sub>c</sub>(x) = max(m<sub>a</sub>(x) - m<sub>b</sub>(x), 0)</code>

**Example:**

`{a,a,b,c} + {a,b,b,c,c} * {a,c,d} = {a,a,a,b,c,c}`

## Simplification Rules:

**Distributivity:**

- `(a * b) + (a * c) → a * (b + c)`
- `(b * a) + (a * c) → a * (b + c)`
- `(a * b) + (b * a) → a * (b + c)`
- `(b * a) + (c * a) → a * (b + c)`

**Rules for Integer calculator REPL:**

- `0 + e → e`
- `e + 0 → e`
- `1 * e → e`
- `e * 1 → e`
- `e * 0 → 0`
- `0 * e → 0`

**Rules for Multiset calculator REPL:**

- `e * e → e`
- `{} * e → {}`
- `e * {} → {}`
- `e + {} → e`
- `{} + e → e`

### Shunting Yard Algorithm
This assignment requires you to parse expressions such as `a + b * c` and deal with operator precedence (i.e. this expression parses to `a + (b * c)`). One way of doing this is by converting the expression into reverse polish notation by using the [Shunting Yard algorithm](http://mathcenter.oxford.emory.edu/site/cs171/shuntingYardAlgorithm/)

### Pretty Printing
We also expect you to "pretty print" a simplified expression such as `((a * b) + (c * d)) as a * b + c * d`. For this, it is handy to realize when parentheses are required: if the current expression is `l op r`, where `l` is the left-sub expression, `r` is the right sub-expression and `op` is the operator, then parentheses are needed around `l` iff:
* `l` is an expression with an operator `op2` AND
* `op2` has lower precedence than `op`

### Operators used and their precedence

| Operator | precedence level |
|:---:     |:----------------:|
| +        | 2                |
| -        | 2                |
| *        | 3                |


## Suggested approach:

1. Completely implement the Integer calculator Repl and make sure it passes all the tests. .
2. Implement a generic, immutable multiset.
3. Implement the Multiset calculator Repl by copying and modifying code from the Integer Repl and making sure it passes all the tests.
4. Your code now works, but has a lot of duplicated code, which hinders maintenance and readability. Refactor your code such that the common parts of both REPLs are shared.

