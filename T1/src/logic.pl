
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


valid_moves(Board, Player, ListOfMoves):-
    getPieces(Player, Pieces),
    valid_moves_piece(Board,Pieces,[], ListOfMoves).