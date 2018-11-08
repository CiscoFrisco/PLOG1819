moves_piece([], RestPieces, Board, Level, BestVal, BestMove):- 
    find_best_move_aux(Board, Level, RestPieces, BestVal, BestMove).


moves_piece([Move | Rest], RestPieces, Board, Level, BestVal, BestMove):-
    move(Move, Board, _NewBoard),
    write(Level),
    minimax(Board, Level, false, MoveVal),     
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

max_best_move(_Best, _Board, _Level, _IsMax, [_]).

max_best_move(Best, Board, Level, IsMax, [Move | Rest]):-
    move(Move, Board, NewBoard),
    minimax(NewBoard,Level,not(IsMax),TempBest),
    NewBest is max(Best, TempBest),
    max_best_move(NewBest, NewBoard, Level, IsMax, Rest).


min_best_move(_Best, _Board, _Level, _IsMax, [_Move | _]).

min_best_move(Best, Board, Level, IsMax, [Move | Rest]):-
    move(Move, Board, NewBoard),
    minimax(NewBoard,Level,not(IsMax),TempBest),
    NewBest is min(Best, TempBest),
    min_best_move(NewBest, NewBoard, Level, IsMax, Rest).

minimax(_Board, 1, _IsMax, _BestVal).

minimax(Board, Level, IsMax, BestVal):-
    value(Board, _Player, Value),
    (Value \= 1, BestVal is Value);
    (
        NewLevel is Level - 1,
        (IsMax,
        (
         Best is -1000,
         valid_moves(Board, 1, ValidMoves),
         max_best_move(Best, Board, NewLevel, IsMax, ValidMoves),
         BestVal is Best
        )
        ;
        (
        Best is 1000,
        valid_moves(Board, 2, ValidMoves),
        min_best_move(Best, Board, NewLevel, IsMax, ValidMoves),
        BestVal is Best
        )   
        )  
    ).

/**
 * Return value for the current state of the game. Used by minimax.
 */ 
value(Board, _Player, Value):-
    game_over(Board, Winner),
    winner(Winner, Value).

/**
 * Return the value for the winner of the game.
 */ 
winner(black, Value) :-
    Value=10.
winner(white, Value):- 
    Value = -10.
winner(none, Value):- 
    Value = 1.
winner(draw, Value):- 
    Value = 0.

choose_move(Board, Level, Move):-
    nextPlayer(Player),
    valid_moves(Board, Player, ValidMoves),
    find_best_move(Board,Level,ValidMoves,Move),
    write(Move).
