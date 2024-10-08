% Examples from lecture P4. Controlling Backtracking and Data Structures
% I. Bratko, Prolog Programming for Artificial Intelligence, 4th edn., Pearson Education 2011.
% Example with CLP(FD)

%% Example – Cryptoarithmetic Puzzle
%       SEND
% +     MORE
% -----------
%       MONEY 

:- use_module(library(clpfd)).       
puzzle([S,E,N,D] + [M,O,R,E] = [M,O,N,E,Y]) :-  
    Vars = [S,E,N,D,M,O,R,Y],     
    Vars ins 0..9,      
    all_different(Vars),        
    % arithmetic constraint that defines the puzzle
    S*1000 + E*100 + N*10 + D
 +  M*1000 + O*100 + R*10 + E
#=
    M*10000 + O*1000 + N*100 + E*10 + Y,
    % M and S start words, and hence cannot be zero 
    M #\= 0, S #\= 0,  
    % attempt to ground as many variables as possible
    label(Vars).
