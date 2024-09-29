% Examples from lecture P4. Controlling Backtracking and Data Structures
% I. Bratko, Prolog Programming for Artificial Intelligence, 4th edn., Pearson Education 2011.

:- style_check(-singleton). % disables a style check of singleton variables

%% An example: state of pollution alert
% Rule 1: if X < 3 then Y = normal
% Rule 2: if 3 ≤ X and X < 6 then Y = alert1
% Rule 3: if 6 ≤ X then Y = alert2

f(X, normal) :- X < 3.             % rule1
f(X, alert1) :- 3 =< X, X < 6.     % rule2
f(X, alert2) :- 6 =< X.            % rule3

%% Pollution alert—Version 2
% The rules are mutually exclusive. One of them at most succeeds. 
% To prevent futile backtracking. The cut (‘!’) mechanism is used
f2(X, normal) :- X < 3, !.         % rule1
f2(X, alert1) :- 3 =< X, X < 6, !. % rule2
f2(X, alert2) :- 6 =< X.           % rule3

% Pollution alert—Version 3: Modified Rules
% More economical formulation
% if X < 3 then Y = normal,
% otherwise if X < 6 then Y = alert1,
% otherwise Y = alert2.
% The redundant conditions are omitted
f3(X, normal) :- X < 3, !.  % rule1
f3(X, alert1) :- X < 6, !.  % rule2
f3(X, alert2).              % rule3

% What happens if the cuts are removed?
f4(X, normal) :- X < 3.  % rule1
f4(X, alert1) :- X < 6.  % rule2
f4(X, alert2).           % rule3 


%% Single solution membership
% Backtracking is prevented as soon as X is found.
member_d(X, [X|Tail]) :- !.
member_d(X, [Head|Tail]) :-  member_d(X, Tail).


%% ”Mary likes any X if X is an animal”
%likes(mary, X) :- animal(X).

% You have to add some clauses for animal/1 and snake/1 to test likes/2
snake(snake1).
animal(cat1).
animal(snake1).


% ”Mary likes all animals but snakes”
% If X is a snake then “Mary likes X” is not true,
% otherwise if X is an animal then “Mary likes X” is true.
likes(mary, X) :-
    snake(X), !, fail.
likes(mary, X) :-
    animal(X).

% A more compact form as one clause
likes2( mary, X) :- 
    snake( X), !, fail
    ; 
    animal( X).
 
% Using negation
likes3(mary, X) :-
  animal(X),
%  not(snake(X)). % can work as well
  \+ snake(X). % '\+' is part of the ISO standard


%% The relation different(X,Y) – X and Y do not match
% if X and Y match then different(X,Y) fails,
% otherwise different(X,Y) succeeds
different(X, X) :- !, fail.
different(X, Y).
 
% Written as one clause
different2(X, Y) :-
  X = Y, !, fail
  ;
  true.

% With negation
different3(X, Y) :-
%  not(X = Y). % can work as well
  \+ X = Y. % '\+' is part of the ISO standard
 

%% Negation and CWA
round(ball).
% if asserted: not(round(table)).
% then error might be issued "No permission to redefine built-in predicate `not/1'"
% if asserted: \+ round(table).
% then error is issued "No permission to modify static procedure `(\+)/1' "


%% Quicksort: Algorithm
% You nedd to add here the definition of conc/3 to test it
% To sort the elements in a non-empty list L in ascending order:
%   Select an arbitrary element X from the list L and 
%     Delete X from L.
%     Split the rest of L into two lists
%       Big containing all elements in L that are greater than X  and
%       Small containing all the remaining elements.
%   Sort Small obtaining SortedSmall.
%   Sort Big obtaining SortedBig.
%   The whole sorted list is the concatenation of SortedSmall and [X|SortedBig].
quicksort([], []).
%simply select the head of the list as X
quicksort([X|Tail], Sorted)  :-
   split(X, Tail, Small, Big),
   quicksort(Small, SortedSmall),
   quicksort(Big, SortedBig),
   conc(SortedSmall, [X|SortedBig], Sorted).

% You also have to define gt/2
split(X, [], [], []).
split(X, [Y|Tail], [Y|Small], Big)  :-
   gt(X, Y), !,
   split(X, Tail, Small, Big).

split(X, [Y|Tail], Small, [Y|Big])  :-
   split(X, Tail, Small, Big).


%% Binary Search Trees
% Implementation of Search in a BST
% To find an item X in a binary dictionary/BST D:
% If X is the root of D then X has been found, otherwise
% If X is less than the root of D then search for X in the left subtree of D, otherwise search for X in the right subtree of D;
% If D is empty the search fails. 
% in(X, T): true if X is a node in a tree T 
in(X, t(_,X,_) ).
in(X, t(L,_,_) ) :-  
  in(X, L).
in(X, t(_,_,R) ) :-  
  in(X, R).

% Insertion in a Binary Dictionary / BST
% addleaf(D, X, D1): adding an item at the leaf level 
% The result of adding X to the empty tree is the tree t(nil, X, nil) .
% If X is the root of D, then D1 = D (no duplicates)
% If the root of D is greater than X, then insert X into the left subtree of D; 
% If the root of D is less than X, then insert X into the right subtree. 
% addleaf( Tree, X, NewTree):
%   inserting X as a leaf into binary dictionary Tree 
%   gives NewTree
 
% the tree is empty
addleaf(nil, X, t( nil, X, nil)).
% no duplicates
addleaf( t( Left, X, Right), X, t( Left, X, Right) ).
% insert X into the left subtree 
addleaf( t( Left, Root, Right), X, t( Left1, Root, Right) )  :-
  gt(Root, X),
  addleaf(Left, X, Left1).
% insert X into the right subtree
addleaf( t( Left, Root, Right), X, t( Left, Root, Right1) )  :-
  gt(X, Root),
  addleaf(Right, X, Right1).


%% Finding a Path in a Graph
% Relation between path and path1
path(A, Z, Graph, Path)  :-
  path1(A, [Z], Graph, Path).
% The start of Path1 coincides with the %start of Path
path1(A, [A | Path1], _, [A | Path1] ).
% otherwise
path1(A, [Y | Path1], Graph, Path)  :-
  adjacent(X, Y, Graph),
  \+ member(X, Path1), %No-cycle condition
  path1(A, [X, Y | Path1], Graph, Path).

% adjacent(X, Y, G):
%     there is an arc from X to Y
%     in graph G 
adjacent(X, Y, graph(Nodes, Edges)) :-
member(e(X,Y), Edges)
;
member(e(Y,X), Edges). 
