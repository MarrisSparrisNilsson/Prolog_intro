% Figure 1.8
% I. Bratko, Prolog Programming for Artificial Intelligence, 4th edn., Pearson Education 2011.
% Four versions of the ancestor program

parent(pam, bob). % Pam is a parent of Bob
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

% The full-name version of ancestor
ancestor(X, Z) :-	% Rule a1: X is ancestor of Z
    parent(X, Z).   % if X is a parent of Z
% a1 defines the direct ancestor relation

ancestor(X, Z) :-	% Rule a2: X is ancestor of Z
    parent(X, Y),   % if X is a parent of Y
    ancestor(Y, Z). % and Y is an ancestor of Z 
% a2 defines the relation in terms of itself

% --------------------------------------------------------------------------------

%% Four versions of the ancestor procedure %%
% The original version
anc1(X, Z) :-   % Rule a1
  parent(X, Z).

anc1(X, Z) :-   % Rule a2
  parent(X, Y),
  anc1(Y, Z).

% --------------------------------------------------------------------------------

% Variation 1: swap clauses of the original version
anc2(X, Z) :-   % Rule a1
  parent(X, Y),
  anc2(Y, Z).

anc2(X, Z) :-   % Rule a2
  parent(X, Z).

% --------------------------------------------------------------------------------


% Variation 2: swap goals in second clause of the original version
anc3(X, Z) :-   % Rule a1
  parent(X, Z).

anc3(X, Z) :-   % Rule a2
  anc3(X, Y),
  parent(Y, Z).

% --------------------------------------------------------------------------------

% Variation 3: swap goals in the second clause and then swap goals of the original version
anc4(X, Z) :-   % Rule a1
  anc4(X, Y),
  parent(Y, Z).

anc4(X, Z) :-   % Rule a2
  parent(X, Z).

% --------------------------------------------------------------------------------


