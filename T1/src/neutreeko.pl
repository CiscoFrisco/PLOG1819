:- use_module(library(lists)).

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

isEmpty(Piece):- Piece=empty.

pvp:-
    nextPlayer(P),
    board(Board),
    retract(nextPlayer(P)),    
    display_game(Board, P),
    choosePieceToMove(InitLine,InitCol,Board,P),
    getValidMoves(InitLine,InitCol,Board, ValidMoves),
    printValidMoves(ValidMoves),
    chooseMove(Move, ValidMoves),
    writeJogada(Jogada, P),
    ((P == 1, assert(nextPlayer(2)));
     (P == 2, assert(nextPlayer(1)))).


%pvb.
%bvb. 

choosePieceToMove(InitLine, InitCol,Board, P) :-
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
    (isWhite(Player, Piece); isBlack(Player, Piece)).

isBlack(Player, Piece):-
    Player = 1,
    Piece = black.

isWhite(Player, Piece):-
    Player = 2,
    Piece = white.

getPiece(LineN, ColN, Board, Piece):-
    nth1(LineN, Board, Line),
    nth1(ColN, Line, Piece).

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

% valid_moves(+Board, +Player, -ListOfMoves) ​

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