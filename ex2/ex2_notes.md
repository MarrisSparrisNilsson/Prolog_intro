# Exercise 2

## Concatenation

The `conc` function works in such a way that:

- You specify the `conc(HEAD, TAIL, [I/O])`.
- **Head** always starts at the **left** of the list
- **Tail** always at the **right** of the list.
- It's a recursive function that takes the first element in the Head and appends it in the output list.
- For each recursion it will be minimized by one element until Head is an empty list.

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

### 2a) Write a goal, using `conc`, to delete the `last three elements` from a list L3 producing another list L1. For example, if `L3=[a, b, c, d, e, f]`, then `L1 = [a, b, c]`

Hint: consider L as the concatenation of L1 and a three-element list L2

```pl
conc(L, [_,_,_],[a,b,c,d,e,f]).
L = [a, b, c]
```

### 2b) Write a goal, using `conc`, to delete the `first three elements` and the `last two elements` from a list L3 producing another list L

```pl
conc([_,_,_|L1], [_,_], [a,b,c,d,e,f,g,h,i,j,k,l]).
L1 = [d,e,f,g,h,i,j].
```
