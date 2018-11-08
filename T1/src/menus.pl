/**
 * Start Player vs Player gamemode and redirect to main menu when it's over.
 */
choose_option(1) :-
    pvp,
    play.

/**
 * Start Player vs Computer gamemode and redirect to main menu when it's over.
 */
choose_option(2):-
     pvb,
     play.

/**
 * Start Computer vs Computer gamemode and redirect to main menu when it's over.
 */
choose_option(3):-
    bvb,
    play.

/**
 * Print rules and return to main menu.
 */
choose_option(4) :-
    print_rules,
    play.

/**
 * Exit game.
 */
choose_option(0) :-
    write('\nExiting game.\n').

/**
 * Displays the game rules on the screen.
 */
print_rules :-
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
print_main_menu :-
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