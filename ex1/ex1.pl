
% EXERCISE 1: PROLOG INTRO

parent(pam, bob). % Pam is a parent of Bob
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).
parent(jim, martin).
parent(martin, joel).

grandchild(Z, X) :- 
    parent(Y, Z),
    parent(X, Y).


has_child(X) :- parent(X,_).
happy(X) :- has_child(X).


% Defines the rules to check data types
check_datatype(Term, Type) :-
    var(Term),         % Check if Term is a variable
    Type = 'variable'.
check_datatype(Term, Type) :-
    integer(Term),     % Check if Term is an integer
    Type = 'integer'.
check_datatype(Term, Type) :-
    float(Term),       % Check if Term is a float
    Type = 'float'.
check_datatype(Term, Type) :-
    atom(Term),        % Check if Term is an atom
    Type = 'atom'.
check_datatype(Term, Type) :-
    compound(Term),    % Check if Term is a compound term
    Type = 'compound term'.
check_datatype(Term, Type) :-
    atomic(Term),      % Check if Term is atomic (either an atom or number)
    Type = 'atomic term'.
check_datatype(Term, Type) :-
    is_list(Term),     % Check if Term is a list
    Type = 'list'.
check_datatype(Term, Type) :-
    nonvar(Term),      % If it's a non-variable and none of the above match, it's something else
    Type = 'unknown type'.

% Helper predicate to check if something is a list
is_list([]).            % The empty list is a list
is_list([_|T]) :-       % A non-empty list is a list if its tail is a list
    is_list(T).


