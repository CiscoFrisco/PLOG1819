/**
 * Start Player vs Player gamemode and redirect to main menu when it's over.
 */
choose_option(1) :-
    clearConsole,
    pvp,
    reset_data,
    play.

/**
 * Start Player vs Computer gamemode and redirect to main menu when it's over.
 */
choose_option(2):-
    clearConsole,
    pvb,
    reset_data,
    play.

/**
 * Start Computer vs Computer gamemode and redirect to main menu when it's over.
 */
choose_option(3):-
    clearConsole,
    bvb,
    reset_data,
    play.

/**
 * Print rules and return to main menu.
 */
choose_option(4) :-
    clearConsole,
    print_rules,
    play.

/**
 * Print rules and return to main menu.
 */
choose_option(5) :-
    clearConsole,
    choose_difficulty,
    play.

/**
 * Exit game.
 */
choose_option(0) :-
    write('\nExiting game.\n').

choose_option(_):-
    play.

/**
 * Displays the game rules on the screen.
 */
print_rules :-
    write('\nWelcome to Neutreeko!\n'),
    write('Each player takes turns. First one plays black. (o)\n'),
    write('In each move, the piece can go in every direction, but it will only stop when it reaches an obstacle, or the edge of the board.\n'),
    write('In order to win, connect your three pieces, in whichever direction.\n'),
    write('Good luck!\n'). 

/**
 * Allows the user to choose bot difficulty.
 */ 
choose_difficulty:-
    difficulty(Difficulty),
    write('\nBot difficulty\n'),
    write('1. Easy\n'),
    write('2. Medium\n'),
    write('3. Hard\n'),
    write('Choose option '),
    read(Option),
    if_then_else((number(Option), Option > 0, Option < 4), (retract(difficulty(Difficulty)), assert(difficulty(Option))), choose_difficulty).

/**
 * Prints the main menu on the screen.
 */ 
print_main_menu :-
    write('\nNeutreeko\n'),
    write('1. Player vs Player\n'),
    write('2. Player vs Computer\n'),
    write('3. Computer vs Computer\n'),
    write('4. Rules\n'),
    write('5. Difficulty\n'),
    write('0. Exit game\n\n').