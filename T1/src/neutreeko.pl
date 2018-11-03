:- consult('display.pl').

:- use_module(library(lists)).
:-dynamic board/1.

board([[empty, white, empty, white, empty],
       [empty, empty, black, empty, empty],
       [empty, empty, empty, empty, empty],
       [empty, empty, white, empty, empty],
       [empty, black, empty, black, empty]]).

:-dynamic nextPlayer/1.

nextPlayer(1).

:- dynamic p1_1/2.
:- dynamic p1_2/2.
:- dynamic p1_3/2.
:- dynamic p2_1/2.
:- dynamic p2_2/2.
:- dynamic p2_3/2.

p1_1(5,2).
p1_2(5,4).
p1_3(2,3).

p2_1(1,2).
p2_2(1,4).
p2_3(4,3).

isEmpty(Piece):- Piece=empty.

isBlack(Player, Piece):-
    Player = 1,
    Piece = black.

isWhite(Player, Piece):-
    Player = 2,
    Piece = white.

pvp:-
    nextPlayer(P),
    board(Board),
    retract(nextPlayer(P)),    
    display_game(Board, P),
    getValidMoves(Board,P,ListOfMoves),
    write('\nHere are the valid Moves:\n'),
    displayValidMoves(ListOfMoves, 1),
    chooseMove(_Option, ListOfMoves, _Move).
    /*move(Move,Board, NewBoard),
    retract(board(Board)),
    assert(board(NewBoard)),
    display_game(NewBoard, P),
    (
        (P == 1, assert(nextPlayer(2)));
        (P == 2, assert(nextPlayer(1)))
    ).*/


%pvb.
%bvb. 

setPiece(1,1,[[_El|Resto1]|Resto2],[[Peca|Resto1]|Resto2],Peca).

setPiece(1,N,[[Elem|Resto1]|Resto2], [[Elem|Head]|Resto2],Peca):- 
	Next is N-1,
	setPiece(1,Next,[Resto1|Resto2],[Head|Resto2],Peca).

setPiece(N, NColuna, [Elem |Resto1],[Elem|Out], Peca):- 
	Next is N-1,
	setPiece(Next,NColuna,Resto1,Out,Peca).

getPiece(LineN,ColN,Board,Piece):-
    nth1(LineN,Board,Line),
    nth1(ColN,Line,Piece).

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


getValidMoves(Board, Player, ListOfMoves):-
    getPieces(Player, Pieces),
    valid_moves_piece(Board,Pieces,[], ListOfMoves).

getChar(Col,Char):-
    TempCol is Col + 64,
    char_code(Char,TempCol).

displayValidMove([InitLine,InitCol,DestLine,DestCol], Counter):-
    write(Counter),write('. '),
    write(InitLine), getChar(InitCol,InitChar),write(InitChar),
    write(' -> '),
    write(DestLine), getChar(DestCol,DestChar),write(DestChar),nl.

displayValidMovesPiece([], Counter,Counter).

displayValidMovesPiece([Head | Tail],Counter, FinalCounter):- 
    displayValidMove(Head, Counter),
    NewCounter is Counter + 1,
    displayValidMovesPiece(Tail,NewCounter, FinalCounter).

displayValidMoves([], _Counter).

displayValidMoves([Head | Tail],Counter):-
    displayValidMovesPiece(Head,Counter,NextCounter),
    displayValidMoves(Tail, NextCounter).


getMovePiece(1, [Head | _Tail], _Pieces, Head).

getMovePiece(Option,[],[_Piece | Rest], Move):-
    getMove(Option, Rest , Move).
    
getMovePiece(Option,[_Head | Tail], [Piece | Rest], Move):-
    NextOption is Option - 1,
    getMovePiece(NextOption, Tail, [Piece | Rest] , Move).

getMove(Option, [Head | Tail], Move):-
    getMovePiece(Option, Head, [Head | Tail], Move).


chooseMove(Option, ListOfMoves,Move):- 
    write('\nMove?'),
    read(Option),
    getMove(Option,ListOfMoves, Move),
    write(Move).
     /*; 
        (write('Please choose a valid option.\n'), chooseMove(NewOption,ListOfMoves,Move))
    ).*/

getPieces(Player, Pieces):-
    ((Player = 1, getBlackPieces(Pieces));
     (Player = 2, getWhitePieces(Pieces))).

getBlackPieces(Pieces):-
    p1_1(A,B),
    p1_2(C,D),
    p1_3(E,F),
    Pieces = [[A,B],[C,D],[E,F]].
    

getWhitePieces(Pieces):-
    p2_1(A,B),
    p2_2(C,D),
    p2_3(E,F),
    Pieces = [[A,B],[C,D],[E,F]].


/*move([InitLine,InitCol,DestLine,DestCol], Board, NewBoard):-
    nextPlayer(Player),
    ((Player = 1, Piece = black);(Piece = white)),
    setPiece(InitLine,InitCol,Board,TempBoard,empty),
    setPiece(DestLine,DestCol,TempBoard,NewBoard, Piece).*/


/*
​game_over(Board, Winner) :- game_over_row(Board, Winner).
​game_over(Board, Winner) :- game_over_col(Board, Winner).
​game_over(Board, Winner) :- game_over_diag(Board, Winner).

game_over_row(Board, Winner).*/


% value(+Board, +Player, -Value)

% choose_move(+Board, +Level, -Move)

play :-
    printMainMenu,
    write('Choose an option '),
    read(Option),
    chooseOption(Option).

chooseOption(1):-
    pvp,
    play.

% chooseOption(2):-
%     pvc,
%     play.

% chooseOption(3):-
%     cvc,
%     play.

chooseOption(4):-
    printRules,
    play.

chooseOption(0):-
    write('\nExiting game.\n').

