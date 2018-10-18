board([[empty, white, empty, white, empty],
       [empty, empty, black, empty, empty],
       [empty, empty, empty, empty, empty],
       [empty, empty, white, empty, empty],
       [empty, black, empty, black, empty]]). 


symbol(empty, S):- S='   '.
symbol(black, S):- S=' o '.
symbol(white, S):- S=' x '.

display_game(Board, Player) :- 
    nl,
    board(Board), 
    show_board(Board, 5), 
    format('~n~nNext player to move: ~d', Player).

show_board([Head | Tail], I):-
    write(' +---+---+---+---+---+'),
    nl,
    write(I),
    write('|'),
    display_line(Head),
    nl,
    NI is I - 1,
    show_board(Tail, NI).
show_board([_], I):-
    write(' +---+---+---+---+---+'),
    nl,
    write('   A   B   C   D   E').

display_line([Head | Tail]):-
    symbol(Head, S),
    write(S),
    write('|'),
    display_line(Tail).
display_line([]).