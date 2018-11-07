
:- (dynamic nextPlayer/1).
nextPlayer(1).

:- (dynamic p1_1/2).
:- (dynamic p1_2/2).
:- (dynamic p1_3/2).
:- (dynamic p2_1/2).
:- (dynamic p2_2/2).
:- (dynamic p2_3/2).

p1_1(5, 2).
p1_2(5, 4).
p1_3(2, 3).

p2_1(1, 2).
p2_2(1, 4).
p2_3(4, 3).

:- (dynamic boards/1).
:- (dynamic countOcorrences/1).
boards([]).
countOcorrences([]).

:- (dynamic board/1).
board([[empty, white, empty, white, empty], [empty, empty, black, empty, empty], [empty, empty, empty, empty, empty], [empty, empty, white, empty, empty], [empty, black, empty, black, empty]]).

set(Piece, Piece).

updatePiece(InitLine,InitCol,DestLine,DestCol,Player):-
    
    (Player = 1,
        (
            (p1_1(A,B), A= InitLine,B=InitCol, retract(p1_1(A,B)), assert(p1_1(DestLine,DestCol)));
            (p1_2(A,B), A= InitLine,B=InitCol, retract(p1_2(A,B)), assert(p1_2(DestLine,DestCol)));
            (p1_3(A,B), A= InitLine,B=InitCol, retract(p1_3(A,B)), assert(p1_3(DestLine,DestCol)))
        )
    );
    (Player = 2,
        (
            (p2_1(A,B), A= InitLine,B=InitCol, retract(p2_1(A,B)), assert(p2_1(DestLine,DestCol)));
            (p2_2(A,B), A= InitLine,B=InitCol, retract(p2_2(A,B)), assert(p2_2(DestLine,DestCol)));
            (p2_3(A,B), A= InitLine,B=InitCol, retract(p2_3(A,B)), assert(p2_3(DestLine,DestCol)))
        )
    ).

setPiece(1,1,[[_El|Resto1]|Resto2],[[Peca|Resto1]|Resto2],Peca).

setPiece(1,N,[[Elem|Resto1]|Resto2], [[Elem|Head]|Resto2],Peca):- 
	Next is N-1,
	setPiece(1,Next,[Resto1|Resto2],[Head|Resto2],Peca).

setPiece(N, NColuna, [Elem |Resto1],[Elem|Out], Peca):- 
	Next is N-1,
	setPiece(Next,NColuna,Resto1,Out,Peca).

/**
 * Performs a move, changing the given piece to a new position, and puts an empty piece on
 * the original one.
 */
move([InitLine, InitCol, DestLine, DestCol], Board, NewBoard) :-
    nextPlayer(Player),
    (   Player=1
    ->  set(black, Piece)
    ;   set(white, Piece)
    ),
    setPiece(InitLine, InitCol, Board, TempBoard, empty),
    setPiece(DestLine, DestCol, TempBoard, NewBoard, Piece),
    updatePiece(InitLine,InitCol,DestLine,DestCol, Player).

isDuplicate([InitLine,InitCol,DestLine,DestCol]):-
    InitLine = DestLine, 
    InitCol = DestCol.

discardDuplicateMoves([], NewList, NewList).

discardDuplicateMoves([Head | Tail], TempList, NewList):-
        (isDuplicate(Head),discardDuplicateMoves(Tail, TempList, NewList));
        (discardDuplicateMoves(Tail, [Head | TempList], NewList)).

valid_moves_piece(_Board,[],ListOfMoves,ListOfMoves).

valid_moves_piece(Board, [Head|Tail],List, ListOfMoves):-
    Init = Head,
    Curr = Head,
    valid_horizontal(Board, Curr, Init, [], HorMoves, -1),
    valid_vertical(Board, Curr, Init, HorMoves, HorVertMoves, -1),
    valid_diagonal(Board, Curr, Init, HorVertMoves, AllMoves, -1, -1),
    discardDuplicateMoves(AllMoves, [], NewAllMoves),
    valid_moves_piece(Board,Tail, [NewAllMoves | List], ListOfMoves).

getPiece(LineN,ColN,Board,Piece):-
    nth1(LineN,Board,Line),
    nth1(ColN,Line,Piece).

/**
 * Returns the player's pieces positions on a list.
 */
getPieces(Player, Pieces) :-
    (   Player=1,
        getBlackPieces(Pieces)
    ;   Player=2,
        getWhitePieces(Pieces)
    ).

/**
 * Returns the black pieces positions on a list.
 */ 
getBlackPieces(Pieces):-
    p1_1(A,B),
    p1_2(C,D),
    p1_3(E,F),
    Pieces = [[A,B],[C,D],[E,F]].
    
/**
 * Returns the white pieces positions on a list.
 */ 
getWhitePieces(Pieces):-
    p2_1(A,B),
    p2_2(C,D),
    p2_3(E,F),
    Pieces = [[A,B],[C,D],[E,F]].

valid_horizontal(_Board, [_Line,_Col], [_InitLine,_InitCol] , Moves, Moves , 3).

valid_horizontal(Board, [Line,Col] , [InitLine,InitCol] , List, Moves , Inc):- 
    NextCol is Col + Inc,
    (
        /*if*/((NextCol = 0 ; NextCol = 6),
                Move = [InitLine, InitCol,Line,Col],
                NextInc is Inc + 2, 
                Next_Col is InitCol,
                valid_horizontal(Board, [Line,Next_Col] , [InitLine,InitCol] , [Move | List] , Moves, NextInc)
              );
    getPiece(Line,NextCol,Board,Piece),
    /*else if*/((Piece = black ; Piece = white), 
                Move = [InitLine, InitCol,Line,Col], 
                NextInc is Inc + 2,
                Next_Col is InitCol,
                valid_horizontal(Board, [Line,Next_Col] , [InitLine,InitCol] , [Move | List] , Moves, NextInc)
               );
    /*else*/(valid_horizontal(Board, [Line,NextCol], [InitLine,InitCol] , List, Moves , Inc))
    ).    

valid_vertical(_Board, [_Line,_Col], [_InitLine,_InitCol] , Moves, Moves , 3).

valid_vertical(Board, [Line,Col] , [InitLine,InitCol] , List, Moves , Inc):- 
    NextLine is Line + Inc,
    (
        /*if*/((NextLine = 0 ; NextLine = 6),
                Move = [InitLine, InitCol,Line,Col],
                NextInc is Inc + 2, 
                Next_Line is InitLine,
                valid_vertical(Board, [Next_Line,Col] , [InitLine,InitCol] , [Move | List] , Moves, NextInc)
              );
    getPiece(NextLine,Col,Board,Piece),
    /*else if*/((Piece = black ; Piece = white),
                Move = [InitLine, InitCol,Line,Col], 
                NextInc is Inc + 2, 
                Next_Line is InitLine,
                valid_vertical(Board, [Next_Line,Col] , [InitLine,InitCol] , [Move | List], Moves , NextInc)
               );
    /*else*/(valid_vertical(Board, [NextLine,Col], [InitLine,InitCol] , List, Moves , Inc))
    ).

valid_diagonal(_Board, [_Line,_Col], [_InitLine,_InitCol] ,Moves, Moves , 3,3).

valid_diagonal(Board, [Line,Col] , [InitLine,InitCol] , List, Moves , LineInc,ColInc):- 
    NextLine is Line + LineInc,
    NextCol  is Col + ColInc,
    (
        /*if*/((NextLine = 0 ; NextLine = 6; NextCol = 0 ; NextCol = 6),
                Move = [InitLine, InitCol,Line,Col],
                (
                 (ColInc < 0 , LineInc < 0,NextColInc is ColInc + 2, NextLineInc is LineInc);
                 (ColInc > 0 , LineInc < 0,NextLineInc is LineInc + 2, NextColInc is ColInc - 2);
                 (ColInc < 0 , LineInc > 0,NextColInc is ColInc + 2, NextLineInc is LineInc);
                 (ColInc > 0 , LineInc > 0,NextLineInc is LineInc + 2, NextColInc is ColInc + 2)
                ),
                Next_Line is InitLine,
                Next_Col is InitCol,
                valid_diagonal(Board, [Next_Line,Next_Col] , [InitLine,InitCol] , [Move | List] , Moves, NextLineInc, NextColInc)
              );
    getPiece(NextLine,NextCol,Board,Piece),
    /*else if*/((Piece = black ; Piece = white),
                Move = [InitLine, InitCol,Line,Col], 
                (
                 (ColInc < 0 , LineInc < 0,NextColInc is ColInc + 2, NextLineInc is LineInc);
                 (ColInc > 0 , LineInc < 0,NextLineInc is LineInc + 2, NextColInc is ColInc - 2);
                 (ColInc < 0 , LineInc > 0,NextColInc is ColInc + 2, NextLineInc is LineInc);
                 (ColInc > 0 , LineInc > 0,NextLineInc is LineInc + 2, NextColInc is ColInc + 2)
                ),
                Next_Line is InitLine,
                Next_Col is InitCol,
                valid_diagonal(Board, [Next_Line,Next_Col] , [InitLine,InitCol] , [Move | List] ,Moves, NextLineInc, NextColInc)
               );
    /*else*/(valid_diagonal(Board, [NextLine,NextCol], [InitLine,InitCol] , List, Moves , LineInc,ColInc))
    ).


valid_moves(Board, Player, ListOfMoves):-
    getPieces(Player, Pieces),
    valid_moves_piece(Board,Pieces,[], ListOfMoves).


/**
 * Checks if there's a winner and returns it. A game is over if someone connected its
 * three pieces horizontally, vertically or diagonally.
 */ 
game_over(_Board, Winner) :- 
    game_over_row(Winner).
game_over(_Board, Winner) :- 
    game_over_col(Winner).
game_over(_Board, Winner) :- 
    game_over_diag(Winner).
game_over(_Board, Winner):-
    game_over_draw(Winner).

/**
 * Checks if three given numbers are consecutive.
 */ 
areNumbersConsecutive(N1, N2, N3) :-
    Min1 is min(N2, N3),
    Min2 is min(N1, Min1),
    Max1 is max(N2, N3),
    Max2 is max(N1, Max1),
    Res is Max2-Min2,
    Res=2.

/**
 * Checks if three given pieces are consecutive in a board line.
 */ 
areConsecutiveHor(Pieces) :-
    nth0(0, Pieces, [F1|F2]),
    nth0(1, Pieces, [S1|S2]),
    nth0(2, Pieces, [T1|T2]),
    F1=S1,
    S1=T1,
    areNumbersConsecutive(F2, S2, T2).

/**
 * Checks if three given pieces are consecutive in a board column.
 */
areConsecutiveVer(Pieces) :-
    nth0(0, Pieces, [F1|F2]),
    nth0(1, Pieces, [S1|S2]),
    nth0(2, Pieces, [T1|T2]),
    F2=S2,
    S2=T2,
    areNumbersConsecutive(F1, S1, T1).

/**
 * Checks if three given pieces are consecutive in a board diagonal.
 */
areConsecutiveDiag(Pieces) :-
    nth0(0, Pieces, [F1|F2]),
    nth0(1, Pieces, [S1|S2]),
    nth0(2, Pieces, [T1|T2]),
    areNumbersConsecutive(F1, S1, T1),
    areNumbersConsecutive(F2, S2, T2).


game_over_draw(Winner):-
    countOcorrences(Count),
    member(3, Count),
    Winner = draw.

game_over_draw(Winner):-
    Winner = none.


/**
 * Checks if a player has three consecutive pieces in a same row, thus winning the game.
 */
game_over_row(Winner) :-
    getBlackPieces(Pieces),
    areConsecutiveHor(Pieces),
    Winner=black.

game_over_row(Winner) :-
    getWhitePieces(Pieces),
    areConsecutiveHor(Pieces),
    Winner=white.

/**
 * Checks if a player has three consecutive pieces in a same diagonal, thus winning the game.
 */
game_over_diag(Winner) :-
    getBlackPieces(Pieces),
    areConsecutiveDiag(Pieces),
    Winner=black.

game_over_diag(Winner) :-
    getWhitePieces(Pieces),
    areConsecutiveDiag(Pieces),
    Winner=white.

/**
 * Checks if a player has three consecutive pieces in a same column, thus winning the game.
 */
game_over_col(Winner) :-
    getBlackPieces(Pieces),
    areConsecutiveVer(Pieces),
    Winner=black.

game_over_col(Winner) :-
    getWhitePieces(Pieces),
    areConsecutiveVer(Pieces),
    Winner=white.