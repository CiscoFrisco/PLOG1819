:- use_module(library(lists)).

:-dynamic board/1.

board([[empty, white, empty, white, empty],
       [empty, empty, black, empty, empty],
       [empty, empty, empty, empty, empty],
       [empty, empty, white, empty, empty],
       [empty, black, empty, black, empty]]).

:-dynamic nextPlayer/1.

nextPlayer(1).

symbol(empty, S):- S='   '.
symbol(black, S):- S=' o '.
symbol(white, S):- S=' x '.

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

pvp:-
    nextPlayer(P),
    board(Board),
    retract(nextPlayer(P)),    
    display_game(Board, P),
    valid_moves(Board,P,Moves),
    readBoardPosition(InitLine,InitCol),
    makeMove(InitLine,InitCol,Board, NewBoard,P),
    retract(board(Board)),
    assert(board(NewBoard)),
    display_game(NewBoard, P),
    /*getValidMoves(InitLine,InitCol,Board, ValidMoves),
    printValidMoves(ValidMoves),
    chooseMove(Move, ValidMoves),
    writeJogada(Jogada, P),*/
    ((P == 1, assert(nextPlayer(2)));
     (P == 2, assert(nextPlayer(1)))).



%pvb.
%bvb. 

/*choosePieceToMove(InitLine, InitCol,Board, P) :-
    write('\nSelect piece to move.\n'),
    readBoardPosition(Line,Col),
    ((checkValidPiece(Line,Col,Board, P),(InitLine = Line, InitCol = Col));
    (write('\nSike! Thats the wroooong piece!'),
    choosePieceToMove(InitLine, InitCol,Board, P))).
translateToBoard(Column, ColumnNumber):-
    char_code(Column, ColumnNumber),
    ColumnNumber is ColumnNumber - 64.

checkValidPiece(Line,Col,Board,Player):-
    getPiece(Line,Col,Board,Piece), 
    (isWhite(Player, Piece); isBlack(Player, Piece)).*/

makeMove(Line,Col,BoardIn, BoardOut, P):-
    ((P = 1, setPiece(Line,Col,BoardIn,BoardOut,black)) ; 
     (P = 2, setPiece(Line,Col,BoardIn,BoardOut,white))),
     write(BoardOut).


setPiece(1,1,[[El|Resto1]|Resto2],[[Peca|Resto1]|Resto2],Peca).
setPiece(1,N,[[Elem|Resto1]|Resto2], [[Elem|Head]|Resto2],Peca):- 
	Next is N-1,
	setPiece(1,Next,[Resto1|Resto2],[Head|Resto2],Peca).

setPiece(N, NColuna, [Elem |Resto1],[Elem|Out], Peca):- 
	Next is N-1,
	setPiece(Next,NColuna,Resto1,Out,Peca).


isBlack(Player, Piece):-
    Player = 1,
    Piece = black.

isWhite(Player, Piece):-
    Player = 2,
    Piece = white.

chooseMove(Move, ValidMoves) :-
    write('Select move '),
    read(Move),
    nth1(Move, ValidMoves, _Elem).

 readBoardPosition(Line,Column):-
    write('Line ?\n'),
    read(Line),
    write('Column ?\n'),
    read(Column).
    %translateToBoard(InitColLetter, InitCol),

getPiece(LineN,ColN,Board,Piece):-
    nth1(LineN,Board,Line),
    nth1(ColN,Line,Piece).

valid_horizontal(Board, [Line,Col], [InitLine,InitCol] , Moves , 3).

valid_horizontal(Board, [Line,Col] , [InitLine,InitCol] , Moves , Inc):- 
    NextCol is Col + Inc,
    (
        /*if*/((NextCol = 0 ; NextCol = 6),(abs(InitCol - NextCol) >  1),
                Move = [InitLine, InitCol,Line,Col],
                write(Move), 
                NextInc is Inc + 2, 
                Next_Col is InitCol,
                valid_horizontal(Board, [Line,NextCol] , [InitLine,InitCol] , [Move | Moves] , NextInc)
              );
    getPiece(Line,NextCol,Board,Piece),
    /*else if*/((Piece = black ; Piece = white), (abs(InitCol - NextCol) > 1),
                Move = [InitLine, InitCol,Line,Col], 
                write(Move), 
                NextInc is Inc + 2, 
                Next_Col is InitCol,
                valid_horizontal(Board, [Line,NextCol] , [InitLine,InitCol] , [Move | Moves] , NextInc)
               );
    /*else*/(valid_horizontal(Board, [Line,NextCol], [InitLine,InitCol] , Moves , Inc))
    ).    

valid_moves_piece(Board,[],ListOfMoves).

valid_moves_piece(Board, [Head|Tail],ListOfMoves):-
    Init = Head,
    Curr = Head,
    valid_horizontal(Board, Curr, Init, Moves, -1),
    write(Moves).
    %valid_vertical()
    %valid_diagonal()
    %valid_moves_piece(Board,Tail,[Moves|ListOfMoves]).

valid_moves(Board, Player, ListOfMoves):-
    getPieces(Board, Player, Pieces),
    valid_moves_piece(Board,Pieces,ListOfMoves).

getPieces(Board, Player, Pieces):-
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


% move(+Move, +Board, -NewBoard)

% ​game_over(+Board, -Winner)

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

printRules :-
    nl,
    write('Welcome to Neutreeko!'),
    nl,
    write('Each player takes turns. First one plays black. (o)'),
    nl,
    write('In each move, the piece can go in every direction, but it will only stop when it reaches an obstacle, or the edge of the board.'),
    nl,
    write('In order to win, connect your three pieces, in whichever direction.'),
    nl,
    write('Good luck!'),
    nl, nl.

printMainMenu:-
    nl,
    write('Neutreeko'), nl,
    write('1. Player vs Player'), nl,
    write('2. Player vs Computer'), nl,
    write('3. Computer vs Computer'), nl,
    write('4. Rules'), nl,
    write('0. Exit game'), nl, nl.

/*
getValidMoves(Line, Column, Board, ValidMoves):- 
    getValidMovesColumn(Line, Column,Board, ValidMoves),
    getValidMovesLine(Line, Column, Board, ValidMoves),
    getValidMovesDiagonal(Line, Column, Board, ValidMoves).

getValidMovesColumn(Line, Column, Board, ValidMoves):-
    nth0(0,Board,Line0),*/

display_game(Board, Player) :-
    nl,
    show_board(Board, 1),
    format('~n~nPlayer ~d is playing.', Player).

show_board([Head | Tail], I):-
    write(' +---+---+---+---+---+'),
    nl,
    write(I),
    write('|'),
    display_line(Head),
    nl,
    NI is I + 1,
    show_board(Tail, NI).
show_board([_], _I):-
    write(' +---+---+---+---+---+'),
    nl,
    write('   A   B   C   D   E').

display_line([Head | Tail]):-
    symbol(Head, S),
    write(S),
    write('|'),
    display_line(Tail).
display_line([]).