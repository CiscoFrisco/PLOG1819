:- consult('display.pl').
:- consult('ai.pl').
:- consult('logic.pl').

:- use_module(library(lists)).

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
        (P = 1, 
            (
                valid_moves(Board, P, ListOfMoves),
                write(ListOfMoves),
                write('\nHere are the valid Moves:\n'),
                displayValidMoves(ListOfMoves, 1),
                chooseMove(ListOfMoves, Move)
            )
        ;
        (
            write('antes\n'),        
            choose_move(Board, 2, Move, P),
            write('depois\n')        
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

bvb_play:-
    nextPlayer(P),
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
chooseOption(3):-
    bvb,
    play.

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

