/**
 * Start Player vs Player gamemode and redirect to main menu when it's over,
 * resetting the database to allow multiple games in a row.
 * 
 * choose_option(+Option)
 */
choose_option(1) :-
    clearConsole,
    pvp,
    reset_data,
    play.

/**
 * Start Player vs Computer gamemode and redirect to main menu when it's over,
 * resetting the database to allow multiple games in a row.
 */
choose_option(2):-
    clearConsole,
    pvb,
    reset_data,
    play.

/**
 * Start Computer vs Computer gamemode and redirect to main menu when it's over,
 * resetting the database to allow multiple games in a row.
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
 * Print difficulty menu and return to main menu.
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

/**
 * In case of invalid input, ask for another option.
 */  
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
 * Allows the user to choose bot difficulty and changes it on the data base.
 * This difficulty reflects on the pvb and bvb gamemodes.
 */ 
choose_difficulty:-
    difficulty(Difficulty),
    write('\n       Bot difficulty\n\n'),
    write('           1. Easy\n'),
    write('           2. Medium\n'),
    write('           3. Hard\n\n'),
    write('Choose option '),
    read(Option),
    if_then_else((integer(Option), Option > 0, Option < 4), (retract(difficulty(Difficulty)), assert(difficulty(Option + 1))), choose_difficulty).

/**
 * Prints the main menu on the screen.
 */ 
print_main_menu :-
    write(' _   _            _                 _         \n'),
    write('| \\ | | ___ _   _| |_ _ __ ___  ___| | _____  \n'),
    write('|  \\| |/ _ \\ | | | __| \'__/ _ \\/ _ \\ |/ / _ \\ \n'),
    write('| |\\  |  __/ |_| | |_| | |  __/  __/   < (_) |\n'),
    write('|_| \\_|\\___|\\__,_|\\__|_|  \\___|\\___|_|\\_\\___/\n\n'),
    write('             1. Player vs Player\n'),
    write('             2. Player vs Computer\n'),
    write('             3. Computer vs Computer\n'),
    write('             4. Rules\n'),
    write('             5. Difficulty\n'),
    write('             0. Exit game\n\n'),
    write('       By: @MrZephyr17 and @CiscoFrisco\n\n').




