:- consult('display.pl').
:- consult('ai.pl').
:- consult('logic.pl').

:- use_module(library(lists)).


:- (dynamic board/1).
board([[empty, white, empty, white, empty], [empty, empty, black, empty, empty], [empty, empty, empty, empty, empty], [empty, empty, white, empty, empty], [empty, black, empty, black, empty]]).

:- (dynamic boards/1).
:- (dynamic countOcorrences/1).

boards([]).
countOcorrences([]).

isEmpty(Piece) :-
    Piece=empty.

isBlack(Player, Piece) :-
    Player=1,
    Piece=black.

isWhite(Player, Piece) :-
    Player=2,
    Piece=white.

pvb_play:-
    nextPlayer(P),
    board(Board),
    display_game(Board, P),
    (
        (P = 1, 
            (
                valid_moves(Board, P, ListOfMoves),
                write('\nHere are the valid Moves:\n'),
                displayValidMoves(ListOfMoves, 1),
                chooseMove(ListOfMoves, Move)
            )
        )
        ;
        (
            choose_move(Board, 1, Move)  
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
    displayValidMoves(ListOfMoves, 1),
    chooseMove(ListOfMoves, Move),
    move(Move, Board, NewBoard),
    retract(nextPlayer(P)),
    retract(board(Board)),
    assert(board(NewBoard)),
    (   P==1,
        assert(nextPlayer(2))
    ;   P==2,
        assert(nextPlayer(1))
    ),
    (
        (   
            write(Boards),
            member(NewBoard, Boards),
            nth0(Index, Boards, NewBoard),
            nth0(Index, CountOcurrences, Count),
            NewCount is Count+1,
            replace(CountOcurrences, Index, NewCount, NewCountOcurrences),
            retract(countOcorrences(CountOcurrences)),
            assert(countOcorrences(NewCountOcurrences))
        )
    ;
        (   append(Boards, [NewBoard], TempNewBoards),
            append(CountOcurrences, [1], TempNewCount),
            retract(boards(Boards)),
            retract(countOcorrences(CountOcurrences)),
            assert(boards(TempNewBoards)),
            assert(countOcorrences(TempNewCount))
        )
    ).

replace([H|T], I, X, [H|R]):- 
    I > 0, 
    I1 is I-1, 
    replace(T, I1, X, R).

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
%bvb. 

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


chooseMove(ListOfMoves,Move):- 
    write('\nMove?'),
    read(Option),
    (getMove(Option,ListOfMoves, Move) -> true ; write('Please choose a valid option.\n'), chooseMove(ListOfMoves,Move)).

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

% https://www.geeksforgeeks.org/minimax-algorithm-in-game-theory-set-2-evaluation-function/

/**
 * Entry point for the game. Prints the main menu, reads user's choice and redirects to
 * the given option.
 */  
play :-
    printMainMenu,
    write('Choose an option '),
    read(Option),
    chooseOption(Option).

/**
 * Start Player vs Player gamemode and redirect to main menu when it's over.
 */
chooseOption(1) :-
    pvp,
    play.

/**
 * Start Player vs Computer gamemode and redirect to main menu when it's over.
 */
chooseOption(2):-
     pvb,
     play.

/**
 * Start Computer vs Computer gamemode and redirect to main menu when it's over.
 */
% chooseOption(3):-
%     cvc,
%     play.

/**
 * Print rules and return to main menu.
 */
chooseOption(4) :-
    printRules,
    play.

/**
 * Exit game.
 */
chooseOption(0) :-
    write('\nExiting game.\n').

