board([[empty, white, empty, white, empty],
       [empty, empty, black, empty, empty],
       [empty, empty, empty, empty, empty],
       [empty, empty, white, empty, empty],
       [empty, black, empty, black, empty]]). 


symbol(empty, S):- S='   '.
symbol(black, S):- S=' o '.
symbol(white, S):- S=' x '.

display_game(Board, Player) :- 
    board(Board), 
    show_board(Board), 
    format('~n~nNext player to move: ~d', Player).

show_board([Head | Tail]):-
    write('+---+---+---+---+---+'),
    nl,
    write('|'),
    display_line(Head),
    nl,
    show_board(Tail).
show_board([_]):-
    write('+---+---+---+---+---+').

display_line([Head | Tail]):-
    symbol(Head, S),
    write(S),
    write('|'),
    display_line(Tail).
display_line([]).