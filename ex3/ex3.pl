:- use_module(library(lists)).


% ############### Controlling backtracking ########################
% --------------- MAX FUNCTIONS ------------------------

max1(X, Y, X) :-
    X >= Y.
max1(X,Y,Y) :-
    X < Y.

max2(X, Y, X) :-
    X >= Y,
    !. 
% "!" skips the backtracking and exits the whole function 
% (similar to a return or break statement which could in some cases
% improve the execution processing performance, but not so much in this case)

% The second call will assume the number in variable Y is the larger one. 
% (Call order sensitive)
max2(_,Y,Y).

max3(X, Y, Max) :-
    X >= Y, !, Max is X
    ;
    Max is Y.
% --------------- MAX FUNCTIONS ------------------------
% --------------- ADD FUNCTIONS ------------------------
% Try #1 maybe not efficient enough?.
member0(_, []).
member0(X, [Head|Tail]) :-
    X \= Head,
    member0(X, Tail).
add0(X, L1, L2) :-
    member0(X, L1), % If X is not a member of L1.
    L2 = [X|L1], !. % Skip next add0 call.
add0(_, L1, L1).

% The solution given in the lectures - deterministic
member_D(X, [X|_]) :- !.
member_D(X, [_|Tail]) :- member_D(X, Tail).

add(X, L1, L1) :- member_D(X, L1), !.
add(X, L1, [X|L1]).

% --------------- ADD FUNCTIONS ------------------------
% --------------- CLASS FUNCTIONS ------------------------
class(Number, 'Positive') :- Number > 0, !.
class(Number, 'Negative') :- Number < 0, !.
class(_, 'Zero').
% --------------- CLASS FUNCTIONS ------------------------
% --------------- SPLIT FUNCTIONS ------------------------
% Define the procedure split(Numbers, Positives, Negatives) which splits a list of numbers into lists: 
% positive ones (including zero) and negative ones. For example: split([3,-1,0,5-2], [3,0,5], [-1,-2]).
% Propose TWO versions: one with a cut and one without. Test both versions with different 
% queries and explore how they work.

% Version 1
split([], [], []).
split([X|L], [X|L1], L2) :- 
    X >= 0, !, % Backtracking disabled. "Cut" the past calls from next call's history and see it as an original call (For easier understanding).
    split(L, L1, L2). % This call is reached regardless of "!", only the call history might differ.
split([X|L], L1, [X|L2]) :- 
    split(L, L1, L2).

% Version 2
split2([], [], []).
split2([X|L], [X|L1], L2) :- 
    X >= 0,
    split2(L, L1, L2).
split2([X|L], L1, [X|L2]) :- 
    X < 0,
    split2(L, L1, L2).

% --------------- SPLIT FUNCTIONS ------------------------
% ############### Controlling backtracking ########################
% ################### Negation ########################
% --------------- FIND_REMAINING FUNCTIONS ------------------------
candidates([person_A,person_B,person_C,person_D,person_F]).
ruled_out([person_B,person_D]).

find_remaining :- 
    candidates(Candidates),
    ruled_out(RuledOut),
    member(X, Candidates), \+ member(X, RuledOut),
    writeln(X).

find_remaining_2 :- 
    candidates(Candidates),
    ruled_out(RuledOut),
    member(X, Candidates), \+ member(X, RuledOut),
    writeln(X), fail.

find_remaining_3 :- 
    candidates(Candidates),
    ruled_out(RuledOut),
    member(X, Candidates), \+ member(X, RuledOut),
    findall(X, 
        (member(X, Candidates), \+ member(X, RuledOut)), 
        List
    ),
    writeln(List).


% Get persons from separate file.
% :- consult(persons).

find_remaining_4 :- 
    candidates(Candidates),
    ruled_out(RuledOut),
    member(X, Candidates), \+ member(X, RuledOut),
    findall(X, 
        (member(X, Candidates), \+ member(X, RuledOut)), 
        List
    ),
    write_persons(List).

write_persons([]).
write_persons([P|Rest]) :- 
    write('Person: '), writeln(P),
    write_persons(Rest).

% --------------- FIND_REMAINING FUNCTIONS ------------------------
% --------------- UNIFIABLE FUNCTIONS ------------------------

% unifiable([X, b, t(Y)], t(a), List).
% List = [X, t(Y)].

unifiable([], _, []).
unifiable([First|Rest], Term, L2) :- 
    \+ First = Term, !,
    unifiable(Rest, Term, L2).
unifiable([First|Rest], Term, [First|L2]) :-
    unifiable(Rest, Term, L2).


% --------------- UNIFIABLE FUNCTIONS ------------------------

% ################### Negation ########################
% ################### Data structures ########################
% --------------- BINARY TREE FUNCTIONS ------------------------

% This should be a valid binary tree object.
t(t(nil, b, nil), a, t(t(nil, d, nil), c, nil )).

% Test if binarytree(Object) is valid tree structure.
binaryTree(nil).
binaryTree(t(Left, _, Right)) :-
    binaryTree(Left),
    binaryTree(Right).

% Compute the height of the binary tree.
height(nil, 0).
height(t(Left, _, Right), H) :-
    % H = H+1,
    height(Left, LH),
    height(Right, RH),
    max2(LH, RH, MaxH),
    H is 1 + MaxH.

% --------------- BINARY TREE FUNCTIONS ------------------------

% ################### Data structures ########################
% ################### Constraint Logic Programming (CLP) ########################

:- use_module(library(clpfd)).

% Call puzzle like this: puzzle(Solution).
% Solution = ([5, 2, 6, 4, 8, 5]+[1, 9, 7, 4, 8, 5]=[7, 2, 3, 9, 7, 0]) .
puzzle([D,O,N,A,L,D] + [G,E,R,A,L,D] = [R,O,B,E,R,T]) :-
    Vars = [D,O,N,A,L,G,E,R,B,T],
    Vars ins 0..9,
    all_different(Vars),
        D*100000 + O*10000 +N*1000 + A*100 + L*10 + D +
        G*100000 + E*10000 +R*1000 + A*100 + L*10 + D #=
    R*100000 + O*10000 + B*1000 + E*100 + R*10 + T,
    D #\= 0, G #\= 0,
    label(Vars). % USE THIS OVER LABELING
    
% ################### Constraint Logic Programming (CLP) ########################
