% Examples from lecture P4. Controlling Backtracking and Data Structures
% I. Bratko, Prolog Programming for Artificial Intelligence, 4th edn., Pearson Education 2011.
% Examples with CLP(R)

%% Temperature Conversion
% version 1
convert(Centigrade, Fahrenheit) :-
    Centigrade is (Fahrenheit-32)*5/9.
% Version 2
%  load the lib for CLP for real numbers 
:- use_module(library(clpr)).
convert2(Centigrade, Fahrenheit) :- 
    { Centigrade = (Fahrenheit-32)*5/9 }.

%% CLP(R): Fibonacci Numbers
% fib( N,F) computes F as the Nth Fibonacci number
% Ver. 1 uses the Prolog standard arithmetic facilities
fib(N,F) :-
    N = 0, F = 1
    ;
    N = 1, F = 1
    ;
    N >= 2,
    N1 is N - 1, fib(N1, F1),
    N2 is N - 2, fib(N2, F2),
    F is F1 + F2.

% Ver 2. fib/2 is rewritten in CLP(R):
:- use_module(library(clpr)).
fib2(N,F) :-
    { N = 0, F = 1 }
    ;
    { N = 1, F = 1 }
    ;
    { N >= 2, F=F1+F2, N1=N-1, N2=N-2 },
    fib2(N1, F1),
    fib2(N2, F2).

% Extra constraints are added
% for all N: F1 >= N1, F2 >= N2.
fib3(N,F) :-
    { N = 0, F = 1 }
    ;
    { N = 1, F = 1 }
    ;
    { N >= 2, F=F1+F2, N1=N-1, N2=N-2,
    F1 >= N1, F2 >= N2 }, % Extra constraints
    fib3(N1, F1),
    fib3(N2, F2).
