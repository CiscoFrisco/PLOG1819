/**
 * Associates a player with a piece symbol.
 */
symbol(empty, S) :-
    S='   '.
symbol(black, S) :-
    S=' o '.
symbol(white, S) :-
    S=' x '.

/**
 * Displays the current board on the screen, and indicates 
 * which player has the current turn.
 */
display_game(Board, Player) :-
    nl,
    show_board(Board, 1),
    format('~n~nPlayer ~d is playing.', Player).

/**
 * Displays the current board on the screen in a user friendly way, 
 * identifying lines and columns.
 */
show_board([Head|Tail], I) :-
    write(' +---+---+---+---+---+'),
    nl,
    write(I),
    write('|'),
    display_line(Head),
    nl,
    NI is I+1,
    show_board(Tail, NI).
show_board([_], _I):-
    write(' +---+---+---+---+---+'),
    nl,
    write('   A   B   C   D   E').

/**
 * Displays a board line on the screen.
 */ 
display_line([Head | Tail]):-
    symbol(Head, S),
    write(S),
    write('|'),
    display_line(Tail).
display_line([]).

display_valid_move([InitLine,InitCol,DestLine,DestCol], Counter):-
    write(Counter),write('. '),
    write(InitLine), get_char_index(InitCol,InitChar),write(InitChar),
    write(' -> '),
    write(DestLine), get_char_index(DestCol,DestChar),write(DestChar),nl.

display_valid_moves_piece([], Counter,Counter).

display_valid_moves_piece([Head | Tail],Counter, FinalCounter):- 
    display_valid_move(Head, Counter),
    NewCounter is Counter + 1,
    display_valid_moves_piece(Tail,NewCounter, FinalCounter).

display_valid_moves([], _Counter).

display_valid_moves([Head | Tail],Counter):-
    display_valid_moves_piece(Head,Counter,NextCounter),
    display_valid_moves(Tail, NextCounter).