/* 1 - Percorrer ValidMoves
   2 - Para cada um, fazer Move no tabuleiro 
   3 - calcular valor com minimax
   4 - desfazer move
   5 - se o valor do move atual é melhor que o bestmove, atualizar
   6 - retornar bestmove
findBestMove(Board, Level, ValidMoves, BestMove):-

minimax(Board, Level, IsMax, BestVal):-
    value(Board, _Player, Value).


choose_move(Board, Level, Move):-
    valid_moves(Board, white, ValidMoves),
    findBestMove(Board,Level,ValidMoves,Move).
*/