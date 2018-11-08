moves_piece([], RestPieces, Board, Level, BestVal, BestMove):- 
    find_best_move_aux(Board, Level, RestPieces, BestVal, BestMove).


moves_piece([Move | Rest], RestPieces, Board, Level, BestVal, BestMove):-
    move(Move, Board, NewBoard),
    minimax(NewBoard, Level, false, MoveVal),     
    write('find_best_move_aux2\n'),
    ( 
        (
            MoveVal > BestVal,
            write('cheguei\n'), 
            moves_piece( Rest, RestPieces, Board, Level, MoveVal, Move)
        );
        moves_piece( Rest, RestPieces, Board, Level, BestVal, BestMove)
    ).

find_best_move_aux(_Board, _Level, [], _BestVal, _BestMove).

find_best_move_aux(Board, Level, [MovesPiece|RestPieces], BestVal, BestMove):-
    moves_piece(MovesPiece, RestPieces, Board,Level, BestVal, BestMove).   

find_best_move(Board, Level, ValidMoves, BestMove):-
    BestVal is -1000,
    find_best_move_aux(Board, Level, ValidMoves, BestVal, BestMove).

max_best_move(_Best, _Board, _Level, _IsMax, []).

max_best_move(Best, Board, Level, IsMax, [MovesPiece | RestPieces]):-
    max_best_move_piece(Best, Board, Level, IsMax, MovesPiece, RestPieces).

min_best_move(_Best, _Board, _Level, _IsMax, []).

min_best_move(Best, Board, Level, IsMax, [MovesPiece | RestPieces]):-
    min_best_move_piece(Best, Board, Level, IsMax, MovesPiece, RestPieces).

max_best_move_piece(Best, Board, Level, IsMax, [], RestPieces):-
    max_best_move(Best,Board,Level,IsMax,RestPieces).

max_best_move_piece(Best, Board, Level, IsMax, [Move | Rest], RestPieces):-
    move(Move, Board, NewBoard),
    minimax(NewBoard,Level,not(IsMax),TempBest),
    NewBest is max(Best, TempBest),
    max_best_move_piece(NewBest, NewBoard, Level, IsMax, Rest,RestPieces).

min_best_move_piece(Best, Board, Level, IsMax, [], RestPieces):-
    min_best_move(Best,Board,Level,IsMax,RestPieces).

min_best_move_piece(Best, Board, Level, IsMax, [Move | Rest], RestPieces):-
    move(Move, Board, NewBoard),
    minimax(NewBoard,Level,not(IsMax),TempBest),
    write(TempBest),nl,
    NewBest is min(Best, TempBest),
    write('hello\n'),
    min_best_move_piece(NewBest, NewBoard, Level, IsMax, Rest, RestPieces).

minimax(_Board, 1, _IsMax, _BestVal).

minimax(Board, Level, IsMax, BestVal):-
    value(Board, _Player, BestVal),
    write('hey:'),
    write(BestVal),nl,
    (BestVal \= 1, true);
    (
        NewLevel is Level - 1 ,
        (IsMax, write('bot1\n'),
            (
            Best is -1000,
            BestVal is Best,
            valid_moves(Board, 1, ValidMoves),
            max_best_move(Best, Board, NewLevel, IsMax, ValidMoves)
            )
        )
        ;
        (   write('bot2\n'),
            Best is 1000,
            BestVal is Best,
            valid_moves(Board, 2, ValidMoves),
            min_best_move(Best, Board, NewLevel, IsMax, ValidMoves)
        )   
    ).

/**
 * Return value for the current state of the game. Used by minimax.
 */ 
value(Board, _Player, Value):-
    game_over(Board, Winner),
    write(Winner),
    winner(Winner, Value).

/**
 * Return the value for the winner of the game.
 */ 
winner(black, 10).

winner(white, -10).

winner(none, 1).
winner(draw, 0).

choose_move(Board, Level, Move):-
    nextPlayer(Player),
    valid_moves(Board, Player, ValidMoves),
    find_best_move(Board,Level,ValidMoves,Move).