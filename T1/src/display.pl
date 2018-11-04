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
 * Displays the game rules on the screen.
 */
printRules :-
    nl,
    write('Welcome to Neutreeko!'),
    nl,
    write('Each player takes turns. First one plays black. (o)'),
    nl,
    write('In each move, the piece can go in every direction, but it will only stop when it reaches an obstacle, or the edge of the board.'),
    nl,
    write('In order to win, connect your three pieces, in whichever direction.'),
    nl,
    write('Good luck!'),
    nl,
    nl.

/**
 * Prints the main menu on the screen.
 */ 
printMainMenu :-
    nl,
    write('Neutreeko'),
    nl,
    write('1. Player vs Player'),
    nl,
    write('2. Player vs Computer'),
    nl,
    write('3. Computer vs Computer'),
    nl,
    write('4. Rules'),
    nl,
    write('0. Exit game'),
    nl,
    nl.

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