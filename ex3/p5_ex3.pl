% Examples from lecture P5. Constraint Logic Programming
% I. Bratko, Prolog Programming for Artificial Intelligence, 4th edn., Pearson Education 2011.
% Examples with CLP(R)


% A CLP(FD) program for eight queens

:- use_module(library(clpfd)).  % constraints library has to be loaded

solution(Ys)  :-             	  % Ys is list of Y-coordinates of queens
  Ys = [ _, _, _, _, _, _, _, _], % There are 8 queens
  Ys ins 1..8,         	 		  % All the coordinates have domains 1..8
  all_different(Ys),       		  % All different to avoid horizontal attacks
  safe(Ys),                 	  % Constrain to prevent diagonal attacks
  labeling([], Ys).         	  % Find concrete values for Ys

% safe(Queens): 
%   Queens is a list of Y-coordinates of non-attacking queens

safe([]).

safe([Queen | Others])  :-
  no_attack(Queen, Others, 1),      	  % 1 = horizontal distance between Queen and Others
  safe(Others).

% no_attack(Y, Ys, Xdist):
%   queen at Y doesn't attack any queen at Ys; 
%   Xdist is column distance between first queen and other queens

no_attack(_Y, [], _).

no_attack(Y1, [Y2 | Ys], Xdist)  :-
  Xdist #\= Y1-Y2,
  Xdist #\= Y2-Y1,
  Xdist1 is Xdist+1,
  no_attack(Y1, Ys, Xdist1).



