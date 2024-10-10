# Exercise 2

## Concatenation

The `conc` function works in such a way that:

-   You specify the `conc(HEAD, TAIL, [I/O])`.
-   **Head** always starts at the **left** of the list
-   **Tail** always at the **right** of the list.
-   It's a recursive function that takes the first element in the Head and appends it in the output list.
-   For each recursion it will be minimized by one element until Head is an empty list.

### Regular concatenation

```pl
conc([a,b], [c,d], L).
L = [a,b,c,d]
```

### List subset

Create a **subset** of the list `L3 = [a,b,c,d,e,f]` containing only `[a,b,c]`

Use `conc` (concatenate):

One way is to pick out all the elements before the element `d`:

```pl
conc(Start, [d|_], [a,b,c,d,e,f]).
Start = [a,b,c].
```

or

```pl
conc(L1, [_,_], [a,b,c,d,e,f]).
Start = [a,b,c].
```

## Exercise Questions

### 1a) Write a goal, using `conc`, to delete the `last three elements` from a list L3 producing another list L1. For example, if `L3=[a, b, c, d, e, f]`, then `L1 = [a, b, c]`

Hint: consider L as the concatenation of L1 and a three-element list L2

```pl
conc(L, [_,_,_],[a,b,c,d,e,f]).
L = [a, b, c]
```

### 1b) Write a goal, using `conc`, to delete the `first three elements` and the `last two elements` from a list L3 producing another list L

```pl
conc([_,_,_|L1], [_,_], [a,b,c,d,e,f,g,h,i,j,k,l]).
L1 = [d,e,f,g,h,i,j].
```

---

### 4b) Use findall/3 to find all solutions with one query

**findall(Template, Goal, Bag):**

Create a list of the instantiations **Template** gets successively on backtracking over **Goal** and unify the result with **Bag**. Succeeds with an empty list if Goal has no solutions. `findall/3` is equivalent to `bagof/3` with all free variables bound with the existential operator (^), except that `bagof/3` fails when Goal has no solutions.

```pl
findall(L2, del_item(a, [a,b,a,a], L2), Results).
Results = [[b, a, a], [a, b, a], [a, b, a]].
```

---

### 5b) Use `setof/3` to find all solutions with one query

```pl
setof(L, insert_item(f, [a,b,c,d], L), Results).
Results = [[a, b, c, d, f], [a, b, c, f, d], [a, b, f, c, d], [a, f, b, c, d], [f, a, b, c|...]].
```

### 12

---

#### Use `listing/1` to get all stored facts

```pl
listing(product).
product(1, 1, 1).
product(1, 2, 2).
product(1, 3, 3).
product(1, 4, 4).
product(1, 5, 5).
||
||
\/
etc.
```

#### Use `retract/1` and `retractall/1` to delete the specified stored fact

```pl
retract(product(_,_,0)).
```

Deletes the first instance where the product is 0.

```pl
retractall(product(_,_,_)).
```

Deletes the entire database.
