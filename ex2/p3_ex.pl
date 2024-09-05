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
