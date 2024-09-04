# Part A: Get Acquainted with the Tools

## Syntax

- Load file: `?- [file_name]` / `make.` (make after each file changes)
- Logical or: `";"`
- Logical and: `","`
- Finish statement: `"."`
- If-statement: `":-"`

## Ask questions

**Is Bob a partent of Pat?:**

`parent(bob,pat). => true`

**Who are bob's parents?:**

```pl
parent(X, bob).
X = pam;
X = tom.
```

**Who are jim's granparents?**

```pl
grandparent(X,jim).
X = bob;
false
```

**Do jim have grandparents?**

`grandparent(_,jim). => true`

**Is jim a grandparent?**

`grandparent(jim,_). => false`

**Who is the common parent of Ann and Pat?**

`parent(X,ann), parent(X,pat). => X = bob.`

## Part B: Rules

### trace

Follow the stack trace of the execution to get a better understanding of how the predicates propagates.

### gtrace

Unkown. Does not work.

### nodebug

Exit debug mode and return to normal predicate execution mode.
