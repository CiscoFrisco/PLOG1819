% Player vs. Player game mode
pvp :-
    pvp_play,
    game_over(Winner),
    clearConsole,
    board(Board),
    handle_winner(Board, Winner, 1). 

% Game turn for Player vs. Player game mode.
pvp_play :-
    nextPlayer(P),
    board(Board),
    boards(Boards),
    countOccurrences(CountOcurrences),
    display_game(Board, P),
    valid_moves(Board, P, ListOfMoves),
    write('\nHere are the valid Moves:\n'),
    display_valid_moves(ListOfMoves, 1),
    choose_player_move(ListOfMoves, Move),
    move(Move, Board, NewBoard),
    retract(nextPlayer(P)),
    retract(board(Board)),
    assert(board(NewBoard)),
    set_next_player(P),
    handle_draw(NewBoard, Boards, CountOcurrences).

% Player vs. Bot gamemode
pvb :-
    pvb_play,
    game_over(Winner),
    %clearConsole,
    board(Board),
    handle_winner(Board, Winner, 2). 

% Game turn for Player vs. Bot gamemode
pvb_play :-
    nextPlayer(P),
    board(Board),
    boards(Boards),
    countOccurrences(CountOcurrences),
    display_game(Board, P),
    make_move(P, Board, NewBoard),
    retract(nextPlayer(P)),
    retract(board(Board)),
    assert(board(NewBoard)),
    set_next_player(P),
    handle_draw(NewBoard, Boards, CountOcurrences).

make_move(1, Board, NewBoard):-
    valid_moves(Board, 1, ListOfMoves),
    write('\nHere are the valid Moves:\n'),
    display_valid_moves(ListOfMoves, 1),
    choose_player_move(ListOfMoves, Move),
    move(Move, Board, NewBoard).

make_move(2, Board, NewBoard):-
    difficulty(Difficulty),
    choose_move(Board, Difficulty, NewBoard).

set_next_player(1):-
    assert(nextPlayer(2)).

set_next_player(2):-
    assert(nextPlayer(1)).

% Bot vs. Bot gamemode
bvb :-
    bvb_play,
    sleep(1),
    game_over(Winner),
    clearConsole,
    board(Board),
    handle_winner(Board, Winner, 3). 

handle_winner(Board, 1, _):-
    show_board(Board, 1),
    write('\n\nPlayer 1 won\n').

handle_winner(Board, 2, _):-
    show_board(Board, 1),
    write('\n\nPlayer 2 won\n'),
    p2_1(P1,P2),write(P1), write('-'),write(P2), nl,
    p2_2(P3,P4),write(P3), write('-'),write(P4),nl,
    p2_3(P5,P6),write(P5), write('-'),write(P6),nl.

handle_winner(Board, -1, _):-
    show_board(Board, 1),
    write('\n\nThere was a draw!\n').

handle_winner(_Board, 0, 3):-
    bvb.

handle_winner(_Board, 0, 2):-
    pvb.

handle_winner(_Board, 0, 1):-
    pvp.
% Game turn for Bot vs. Bot gamemode
bvb_play :-
    nextPlayer(P),
    board(Board),
    boards(Boards),
    countOccurrences(CountOcurrences),
    difficulty(Difficulty),
    display_game(Board, P),
    choose_move(Board,Difficulty, NewBoard),
    retract(nextPlayer(P)),
    retract(board(Board)),
    assert(board(NewBoard)),
    set_next_player(P),
    handle_draw(NewBoard, Boards, CountOcurrences).
