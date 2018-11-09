pvp :-
    pvp_play,
    board(Board),
    game_over(Board, Winner),
    (
        (
            Winner=black,
            write('\nPlayer 1 won\n'),
            show_board(Board, 1),
            resetData
        )
    ;   ( 
            Winner=white,
            write('\nPlayer 2 won\n'),
            show_board(Board, 1),
            resetData
        )
    ;   (
            Winner=draw,
            write('\nThere was a draw!\n'),
            show_board(Board, 1),
            resetData
        )
    ;   pvp
    ). 

pvb :-
    pvb_play,
    board(Board),
    game_over(Board, Winner),
    (   Winner=black,
        write('\nPlayer 1 won\n'),
        show_board(Board, 1)
        % resetData
    ;   Winner=white,
        write('\nAI won\n'),
        show_board(Board, 1)
        % resetData
    ;   Winner=draw,
        write('\nThere was a draw!\n'),
        show_board(Board, 1)
        % resetData
    ;   pvb
    ). 

bvb_play :-
    nextPlayer(P),
    board(Board),
    display_game(Board, P),
    choose_move(Board,2, Move, P),
    move(Move, Board, NewBoard),
    retract(nextPlayer(P)),
    retract(board(Board)),
    assert(board(NewBoard)),
    (   P==1,
        assert(nextPlayer(2))
    ;   P==2,
        assert(nextPlayer(1))
    ).
            

bvb :-
    bvb_play,
    board(Board),
    game_over(Board, Winner),
    (   Winner=black,
        write('\nPlayer 1 won\n'),
        show_board(Board, 1)
        % resetData
    ;   Winner=white,
        write('\nPlayer 2 won\n'),
        show_board(Board, 1)
        % resetData
    ;   Winner=draw,
        write('\nThere was a draw!\n'),
        show_board(Board, 1)
        % resetData
    ;   bvb
    ). 

pvb_play :-
    nextPlayer(P),
    difficulty(Difficulty),
    board(Board),
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
    ).

/**
 * Player vs player gamemode.
 */
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