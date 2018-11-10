% Player vs. Player game mode
pvp :-
    pvp_play,
    board(Board),
    game_over(Board, Winner),
    (
        (
            Winner=black,
            write('\nPlayer 1 won\n'),
            show_board(Board, 1)
        )
    ;   ( 
            Winner=white,
            write('\nPlayer 2 won\n'),
            show_board(Board, 1)
        )
    ;   (
            Winner=draw,
            write('\nThere was a draw!\n'),
            show_board(Board, 1)
        )
    ;   pvp
    ). 

% Game turn for Player vs. Player game mode.
pvp_play :-
    nextPlayer(P),
    board(Board),
    boards(Boards),
    countOcorrences(CountOcurrences),
    display_game(Board, P),
    valid_moves(Board, P, ListOfMoves),
    write('\nHere are the valid Moves:\n'),
    display_valid_moves(ListOfMoves, 1),
    choose_player_move(ListOfMoves, Move),
    move(Move, Board, NewBoard),
    retract(nextPlayer(P)),
    retract(board(Board)),
    assert(board(NewBoard)),
    (   P==1,
        assert(nextPlayer(2))
    ;   P==2,
        assert(nextPlayer(1))
    ),
    handle_draw(NewBoard, Boards, CountOcurrences).

% Player vs. Bot gamemode
pvb :-
    pvb_play,
    board(Board),
    game_over(Board, Winner),
    (
        (   Winner=black,
        write('\nPlayer 1 won\n'),
        show_board(Board, 1)
        )
    ;   (Winner=white,
        write('\nAI won\n'),
        show_board(Board, 1)
    )
    ;   (Winner=draw,
        write('\nThere was a draw!\n'),
        show_board(Board, 1)
    )
    ;   pvb
    ). 

% Game turn for Player vs. Bot gamemode
pvb_play :-
    nextPlayer(P),
    difficulty(Difficulty),
    board(Board),
    boards(Boards),
    countOcorrences(CountOcurrences),
    display_game(Board, P),
    (
        (   
            P=1,
            valid_moves(Board, P, ListOfMoves),
            write('\nHere are the valid Moves:\n'),
            display_valid_moves(ListOfMoves, 1),
            choose_player_move(ListOfMoves, Move)
        )
        ;  
        ( 
            choose_move(Board, Difficulty, Move)
        )
    ),
    move(Move, Board, NewBoard),
    retract(nextPlayer(P)),
    retract(board(Board)),
    assert(board(NewBoard)),
    (   P==1,
        assert(nextPlayer(2))
    ;   P==2,
        assert(nextPlayer(1))
    ),
    handle_draw(NewBoard, Boards, CountOcurrences).

% Bot vs. Bot gamemode
bvb :-
    bvb_play,
    board(Board),
    game_over(Board, Winner),
    (   
        (Winner=black,
        write('\nPlayer 1 won\n'),
        show_board(Board, 1)
        )
    ;  
    ( Winner=white,
        write('\nPlayer 2 won\n'),
        show_board(Board, 1)
    )
    ;   
    (Winner=draw,
        write('\nThere was a draw!\n'),
        show_board(Board, 1))
    ;   bvb
    ). 

% Game turn for Bot vs. Bot gamemode
bvb_play :-
    nextPlayer(P),
    board(Board),
    boards(Boards),
    countOcorrences(CountOcurrences),
    difficulty(Difficulty),
    display_game(Board, P),
    choose_move(Board,Difficulty, Move),
    move(Move, Board, NewBoard),
    retract(nextPlayer(P)),
    retract(board(Board)),
    assert(board(NewBoard)),
    (   P==1,
        assert(nextPlayer(2))
    ;   P==2,
        assert(nextPlayer(1))
    ),handle_draw(NewBoard, Boards, CountOcurrences).
