# Exercise 3

## Constraint Logic Programming (CLP)

If you want extra information about the clpfd library type the following in the command prompt:

```pl
?- help(clpfd).
```

## Operators

### '='

**Purpose:** Tests whether two terms are `unifiable`.

**Behavior:**

- If the terms are already identical or can be made identical through unification, the operation succeeds.
- Does not evaluate expressions; it works on terms as they are.

### 'is'

**Purpose:** Evaluates the right-hand side as an arithmetic expression and assigns the result to the left-hand side (if the left-hand side is a variable).

**Behavior:**

- The right-hand side must be evaluatable (e.g., numbers, arithmetic expressions).
- The left-hand side must be a variable or a number that matches the result of the evaluation.

### !

The cut operator `!` in Prolog is a control construct that affects the flow of the program. It is used to prune the search tree and prevent backtracking beyond the point where it is encountered. This can make programs more efficient but also alters the logical behavior of the program.

- When Prolog encounters a cut `!`, it commits to the choices made up to that point.
- Prolog will not consider alternative rules or clauses of the same predicate after a cut succeeds.

"Cut" the past calls from next call's history (see it as an original call for easier understanding).

Example:

```pl
test(1) :- !, write('First clause'), nl.
test(2) :- write('Second clause'), nl.

-? text(X).
First clause
X = 1.
```

### \\+

The `\+` operator is a way to test whether a goal cannot be proven. It's a fundamental part of Prolog's **"negation as failure"** approach, but it relies on the state of the knowledge base and Prolog's ability to derive solutions. Use it cautiously when dealing with **incomplete** or **variable-dependent** data.

## Unification

In Prolog, an element is unifiable with a term if there exists a way to make them identical by substituting variables in either the element or the term (or both).

An element is unifiable with a term if:

1. They have compatible structures.
2. Variables can be instantiated in a way that makes them identical.

Example of Unification:

```pl
X = 5.  % X unifies with 5 by binding X to 5.

X = Y.  % X and Y unify by becoming equal to each other.

t(X, 3) = t(4, Y).  % X unifies with 4, and Y unifies with 3.

[H|T] = [1, 2, 3].  % H unifies with 1, and T unifies with [2, 3].

3 = 4.  % This fails because 3 and 4 are distinct constants.
```
