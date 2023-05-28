:- use_module(library(clpfd)).

read_and_solve_sudoku(File, Solution) :-
    open(File, read, Stream),
    read_lines(Stream, InitialGrid),
    sudoku(Solution, InitialGrid),
    maplist(label, Solution),
    close(Stream).

read_lines(Stream, []) :- at_end_of_stream(Stream).

read_lines(Stream, [Line|Rest]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, LineStr),
    split_string(LineStr, " ", " ", LineList),
    maplist(atom_number, LineList, LineNumList),
    Line = LineNumList,
    read_lines(Stream, Rest).

sudoku(Solution, InitialGrid) :-
        length(Solution, 9),
        maplist(same_length(Solution), Solution),
        append(Solution, Values), Values ins 1..9,
        maplist(all_distinct, Solution),
        transpose(Solution, Columns),
        maplist(all_distinct, Columns),
        Solution = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
        distinct_blocks(As, Bs, Cs),
        distinct_blocks(Ds, Es, Fs),
        distinct_blocks(Gs, Hs, Is),
        same_initial_elements(InitialGrid, Solution).

distinct_blocks([], [], []).
distinct_blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
        all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
        distinct_blocks(Ns1, Ns2, Ns3).

same_initial_elements(List1, List2) :- maplist(same_initial_row_elements, List1, List2).
same_initial_row_elements([], []).
same_initial_row_elements([X|Xs], [X|Ys]) :- X \= 0, same_initial_row_elements(Xs, Ys).
same_initial_row_elements([0|Xs], [Y|Ys]) :- Y \= 0, same_initial_row_elements(Xs, Ys).