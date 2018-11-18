/**
 * Associates a piece with its symbol.
 * 
 * symbol(+Piece, -Symbol)
 */
symbol(empty, '   ').
symbol(black, ' o ').
symbol(white, ' x ').

/**
 * Clears sicstus console for more fluid gameplay.
 */  
clearConsole:-
    write('\33\[2J').

/**
 * Displays the current board on the screen, and indicates 
 * which player has the current turn.
 * 
 * display_game(+Board, +Player)
 */
display_game(Board, Player) :-
    nl,
    show_board(Board, 1),
    format('~n~nPlayer ~d is playing.~n', Player).

/**
 * Displays the current board on the screen in a user friendly way, 
 * identifying lines and columns.
 * 
 * show_board(+Board, +Line)
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
 * 
 * display_line(+Line)
 */ 
display_line([Head | Tail]):-
    symbol(Head, S),
    write(S),
    write('|'),
    display_line(Tail).
display_line([]).

/**
 * Display a valid move for the player.
 * 
 * display_valid_move(+Move, +Counter)
 */ 
display_valid_move([InitLine,InitCol,DestLine,DestCol], Counter):-
    write(Counter),write('. '),
    write(InitLine), get_char_index(InitCol,InitChar),write(InitChar),
    write(' -> '),
    write(DestLine), get_char_index(DestCol,DestChar),write(DestChar),nl.

/**
 * Display valid moves for the player.
 * 
 * display_valid_moves(+ValidMoves, +Counter)
 */ 
display_valid_moves([], _Counter).

display_valid_moves([Head | Tail],Counter):-
    display_valid_move(Head, Counter),
    NextCounter is Counter + 1,
    display_valid_moves(Tail, NextCounter).