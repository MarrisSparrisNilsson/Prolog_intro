% Figure 1.8
% I. Bratko, Prolog Programming for Artificial Intelligence, 4th edn., Pearson Education 2011.

% ----------------- Facts -----------------

parent(pam, bob). % Pam is a parent of Bob
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

female(pam).      % Pam is female
female(liz).
female(ann).
female(pat).

male(tom).        % Tom is male
male(bob).
male(jim).

mother(X, Y) :-	 % X is the mother of Y if
  parent(X, Y),	 % X is a parent of Y and
  female(X).     % X is female

grandparent(X, Z) :- % X is a grandparent of Z if
  parent(X, Y),      % X is a parent of Y and
  parent(Y, Z).      % Y is a parent of Z

% ----------------- Facts -----------------

happy(X) :- parent(X,_).

grandchild(X,Y) :- parent(Z,X), parent(Y, Z).

sister(X,Y) :-
    parent(ParentName,X), 
    parent(ParentName,Y),
    female(X), 
    X \= Y.

aunt(X,Y) :-
    sister(X,Z),
    parent(Z,Y).


