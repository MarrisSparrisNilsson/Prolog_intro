% 2.
% a)

last(Item, List) :-
    conc(_, [Item], List).


% b)
last(Item, [Last]).
last(Item, [List]) :-
    last(_, [_,Item]).


% 3.
addItem(Item, L1, [Item|L1]).


% 4. 
% del_item(Item, L1, [Item,L1]) :-


    