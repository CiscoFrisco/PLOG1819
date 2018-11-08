pvp :-
    pvp_play,
    board(Board),
    game_over(Board, Winner),
    (   Winner=black,
        write('Player 1 won\n')
    ;   Winner=white,
        write('Player 2 won\n')
    ;   Winner=draw,
        write('There was a draw!\n')
    ;   pvp
    ). 

pvb :-
    pvb_play,
    board(Board),
    game_over(Board, Winner),
    (   Winner=black,
        write('Player 1 won\n')
    ;   Winner=white,
        write('AI won\n')
    ;   Winner=draw,
        write('There was a draw!\n')
    ;   pvb
    ). 

bvb_play :-
    next_player(P),
    board(Board),
    display_game(Board, P),
    choose_move(Board, 1, Move, P),
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
        write('Player 1 won\n')
    ;   Winner=white,
        write('AI won\n')
    ;   Winner=draw,
        write('There was a draw!\n')
    ;   bvb
    ). 

pvb_play :-
    next_player(P),
    board(Board),
    display_game(Board, P),
    (   P=1,
        valid_moves(Board, P, ListOfMoves),
        write('\nHere are the valid Moves:\n'),
        display_valid_moves(ListOfMoves, 1),
        choose_player_move(ListOfMoves, Move)
    ;   write('antes\n'),
        choose_move(Board, 2, Move),
        write('depois\n')
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
    next_player(P),
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