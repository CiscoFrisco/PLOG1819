:- consult('logic.pl').

not(X):- X, !, fail.
not(_X).

movesPiece([], RestPieces, Board, Level, BestVal, BestMove):- 
    siga(Board, Level, RestPieces, BestVal, BestMove).


movesPiece([Move | Rest], RestPieces, Board, Level, BestVal, BestMove):-
    move(Move, Board, _NewBoard),
    write(Level),
    minimax(Board, Level, false, MoveVal),     
    write('siga2\n'),
    ( 
        (
            MoveVal > BestVal,
            write('cheguei\n'), 
            movesPiece( Rest, RestPieces, Board, Level, MoveVal, Move)
        );
        movesPiece( Rest, RestPieces, Board, Level, BestVal, BestMove)
    ).

siga(_Board, _Level, [], _BestVal, _BestMove).

siga(Board, Level, [MovesPiece|RestPieces], BestVal, BestMove):-
    movesPiece(MovesPiece, RestPieces, Board,Level, BestVal, BestMove).   

findBestMove(Board, Level, ValidMoves, BestMove):-
    BestVal is -1000,
    siga(Board, Level, ValidMoves, BestVal, BestMove).

maxBestMove(_Best, _Board, _Level, _IsMax, [_]).

maxBestMove(Best, Board, Level, IsMax, [Move | Rest]):-
    move(Move, Board, NewBoard),
    minimax(NewBoard,Level,not(IsMax),TempBest),
    NewBest is max(Best, TempBest),
    maxBestMove(NewBest, NewBoard, Level, IsMax, Rest).


minBestMove(_Best, _Board, _Level, _IsMax, [_Move | _]).

minBestMove(Best, Board, Level, IsMax, [Move | Rest]):-
    move(Move, Board, NewBoard),
    minimax(NewBoard,Level,not(IsMax),TempBest),
    NewBest is min(Best, TempBest),
    minBestMove(NewBest, NewBoard, Level, IsMax, Rest).

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
         maxBestMove(Best, Board, NewLevel, IsMax, ValidMoves),
         BestVal is Best
        )
        ;
        (
        Best is 1000,
        valid_moves(Board, 2, ValidMoves),
        minBestMove(Best, Board, NewLevel, IsMax, ValidMoves),
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
    findBestMove(Board,Level,ValidMoves,Move),
    write(Move).
