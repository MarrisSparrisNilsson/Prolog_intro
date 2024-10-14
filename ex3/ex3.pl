max1(X, Y, X) :-
    X >= Y.
max1(X,Y,Y) :-
    X < Y.

max2(X, Y, X) :-
    X >= Y,
    !.
max2(_,Y,Y).

max3(X, Y, Max) :-
    X >= Y, !, Max is X
    ;
    Max is Y.
