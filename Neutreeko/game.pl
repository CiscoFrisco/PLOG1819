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

explain:-
    write('Welcome to Neutreeko!'),
    nl,
    write('Each player takes turns. First one plays black. (o)'),
    nl,
    write('In each move, the piece can go in every direction, but it will only stop when it reaches an obstacle.'),
    nl,
    write('Good luck!'),
    nl.

pvp:-
    explain,
    nl,
    nextPlayer(P),
    retract(nextPlayer(P)),    
    display_game(_, P),
    readJogada(Line, Column),
    
    generateJogadas(Line,Column,Board),
    read(Jogada),
    writeJogada(Jogada, P),
    ((P = 1, assert(nextPlayer(2)));
     (P = 2, assert(nextPlayer(1)))).


%pvb.
%bvb. 

readJogada(Line,Column):-
    read(Column),
    translateToBoard(Column),
    read(Line).

translateToBoard(Column):-
    char_code(Column, ColumnNumber),
    ColumnNumber is ColumnNumber - 65.



/*
generateJogadas(Line, Column, Board):- 
    generateJogadascolumn(Line, Column,Board),
    generateJogadasline(Line, Column, Board),
    generateJogadasdiagonal(Line, Column, Board).

generateJogadascolumn(Line, Column, Board):-
    */

display_game(Board, Player) :- 
    nl,
    board(Board), 
    show_board(Board, 5), 
    format('~n~nPlayer ~d to move (ColumnLine):', Player).

show_board([Head | Tail], I):-
    write(' +---+---+---+---+---+'),
    nl,
    write(I),
    write('|'),
    display_line(Head),
    nl,
    NI is I - 1,
    show_board(Tail, NI).
show_board([_], I):-
    write(' +---+---+---+---+---+'),
    nl,
    write('   A   B   C   D   E').

display_line([Head | Tail]):-
    symbol(Head, S),
    write(S),
    write('|'),
    display_line(Tail).
display_line([]).