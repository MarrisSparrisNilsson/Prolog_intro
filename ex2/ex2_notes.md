# Exercise 2

## Concatenation

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

### Advanced operations

Write a goal, using `conc`, to delete the `first three elements` and the `last two elements` from a list L3 producing another list L`.

```pl
conc([_,_,_|L1], [_,_], [a,b,c,d,e,f,g,h,i,j,k,l]).
L1 = [d,e,f,g,h,i,j].
```
