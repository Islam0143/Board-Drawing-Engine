:- use_module(library(clpfd)).

board_size(8).

queens8(Board) :-
  board_size(N),
  length(Board, N),
  Board ins 1..N,
  all_distinct(Board),
  no_attack(Board),
  labeling([], Board).

no_attack([]).
no_attack([Q|Rest]) :-
  safe(Q, Rest, 1),
  no_attack(Rest).

safe(_, [], _).
safe(Row, [NextRow|Rest], Diagonal) :-
  Row #\= NextRow + Diagonal,
  Row #\= NextRow - Diagonal,
  NextDiagonal = Diagonal + 1,
  safe(Row, Rest, NextDiagonal).