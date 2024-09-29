% Examples from lecture P3.
% I. Bratko, Prolog Programming for Artificial Intelligence, 4th edn., Pearson Education 2011.


%% Definition of a List 
% define a predicate to evaluate whether a term is a list
% A term is a list if it is an empty list.
is_a_list([]).   
% A term is a list if it has two elements and the second is a list.
is_a_list([_|T]):- is_a_list(T).   

%% Membership Implementation
% member( X, L): X is member of L
member_2( X, [ X | _ ]).  % X appears as the head of list
member_2( X, [ _ | L]) :- % X in the tail of list
    member_2( X, L).  

%% Length
% If the list is empty then its length is 0
length2([ ], 0).
% If it is not empty then its lengths equal to 1 plus the length of the tail
length2([ _ | L], N) :- 
    length2( L, N1),  N is N1 + 1.

%% Concatenation
% If the first argument is an empty list, 
% then the second and third arguments must be the same list
conc([], L, L).
% If the first argument is a non-empty list 
% then it has a head and a tail: [X|L1].
% The result is the list [X|L3], where L3 is the concatenation of L1 and L2
conc([X|L1], L2, [X|L3]) :- 
    conc(L1, L2, L3).

% The membership relation can implemented using conc
member_3(X, L) :- conc(_, [X|_], L).

%% Example of an Operator Definition
:-op(300, xfx, plays).
:-op(200, xfy, and).
jimmy plays football and squash.
susan plays tennis and basketball and volleyball.

%% Enlargement of Figures
% enlarge(Fig, Factor, Fig1).
enlarge(square(A), F, square(A1)) :-
    A1 is F*A.
enlarge(circle(R), F, circle(R1)) :-
    R1 is F*R.
enlarge(rectangle(A ,B), F, rectangle(A1, B1)) :-
    A1 is F*A, B1 is F*B.
% Enlargement with the univ operator 
enlarge2(Fig, F, Fig1) :-  
    Fig =.. [Type|Parameters],  
    multiplylist(Parameters, F, Parameters1),  
    Fig1 =.. [Type|Parameters1].

multiplylist([], _, []).
multiplylist([X|L], F, [X1|L1]) :-
    X1 is F*X,   multiplylist(L, F, L1).
 
%% Collecting all Solutions
% a list of people 
age(peter, 17).
age(ann, 25).
age(pat, 18).
age(tom, 25).

/* A program that simulates the robot action
   of moving a block from one place to another place */
   :- dynamic on/2.
%move Block from Loc1 to Loc2
move(Block, Loc1, Loc2):-
  retract(on(Block, Loc1)), %Block is no longer on Loc1
  assert(on(Block, Loc2)).  %Block is now on Loc2. 

on(a,b).
on(b,table).
on(c,table).

% ================================= Exercise answers ==============================

% ========== PART A ========== 

% 2.
% a) With conc

% last(Item, List) :- conc(_, [Item], List).

% b) Without conc
last(Item, [Item]).
last(Item, [_|Last]) :- 
    last(Item, Last).


% 3.
addItem(Item, L1, [Item|L1]).


% 4. 
del_item(Item, [Item|Tail], Tail).
del_item(Item, [Y|Tail], [Y|Taill]) :-
    del_item(Item, Tail, Taill).


% 5.
% insert_item(X, L1, L2) :- 

insert_item(Item, List, InsertedItemList) :-
    del_item(Item, InsertedItemList, List).


% 6.
means(0, zero).
means(1, one).
means(2, two).
means(3, three).
means(4, four).
means(5, five).
means(6, six).
means(7, seven).
means(8, eight).
means(9, nine).

% [4,6,2,3], [Translation]
translate([],[]).
translate([Head1|Tail1], [Head2|Tail2]) :-
    means(Head1, Head2),
    translate(Tail1, Tail2).

% Print translation on seperate line
fix_output([]).
fix_output([Head|Tail]) :-
    writeln(Head),
    fix_output(Tail).

translate_input :-
    writeln("Enter a list of numbers to translate: "), read(List),
    translate(List, Translation),
    writeln("The translated list is:"), 
    fix_output(Translation).
    

% 7. 
reverse([],[]).
reverse([First|Rest], Reversed) :-
    reverse(Rest, ReversedRest),
    conc(ReversedRest, [First], Reversed).
    

% ========== PART B ========== 

% 8.

:-op(300, xfx, was).
:-op(200, xfx, of).
:-op(100, fx, the).
diana was the secretary of the department.

% 9.

% :-op(100, xfx, delelting).

:-op(500, xfx, in).
:-op(500, xfx, gives).
:-op(400, fx, concatenating).
:-op(300, xfy, and).
:-op(400, fx, deleting).
:-op(300, xfx, from).

% List membership
% X appears as the head of list
% X in the tail of list
Item in [Item | _ ].
Item in [_ | Rest] :- 
    Item in Rest.

% List concatenation
concatenating [] and List gives List.
concatenating [Head1|Tail1] and List2 gives [Head1|List3] :-
    concatenating Tail1 and List2 gives List3.

% Deleting from a list
deleting Item from [Item | Rest] gives Rest.
deleting Item from [First | Rest] gives [First | NewRest] :-
    deleting Item from Rest gives NewRest.


% ========== PART C ========== 

% 10.