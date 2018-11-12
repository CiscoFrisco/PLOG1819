% Indicates which player has the next turn (1/2)
:- (dynamic nextPlayer/1).
nextPlayer(1).

% AI difficulty (EASY / MEDIUM / HARD), reflecting on minimax depth
:- (dynamic difficulty/1).
difficulty(2).

% Variables that hold each piece position on the board (i, j)
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


% Variables to support checking for a draw
:- (dynamic boards/1).
:- (dynamic countOcorrences/1).
boards([]).
countOcorrences([]).

% The game's board, represented by a list of lists
:- (dynamic board/1).
board([[empty, white, empty, white, empty], [empty, empty, black, empty, empty], [empty, empty, empty, empty, empty], [empty, empty, white, empty, empty], [empty, black, empty, black, empty]]).

% Resets main variables to initial state, to support consecutive games
resetData :-
    board(Board),
    retract(board(Board)),
    assert(board(
                 [ [empty, white, empty, white, empty],
                   [empty, empty, black, empty, empty],
                   [empty, empty, empty, empty, empty],
                   [empty, empty, white, empty, empty],
                   [empty, black, empty, black, empty]
                 ])),
    boards(Boards),
    retract(boards(Boards)),
    assert(boards([])),
    countOcorrences(CountOcorrences),
    retract(countOcorrences(CountOcorrences)),
    assert(countOcorrences([])),
    nextPlayer(Player),
    retract(nextPlayer(Player)),
    assert(nextPlayer(1)),
    p1_1(A, B),
    retract(p1_1(A, B)),
    assert(p1_1(5, 2)),
    p1_2(C, D),
    retract(p1_2(C, D)),
    assert(p1_2(5, 4)),
    p1_3(E, F),
    retract(p1_3(E, F)),
    assert(p1_3(2, 3)),
    p2_1(G, H),
    retract(p2_1(G, H)),
    assert(p2_1(1, 2)),
    p2_2(I, J),
    retract(p2_2(I, J)),
    assert(p2_2(1, 4)),
    p2_3(K, L),
    retract(p2_3(K, L)),
    assert(p2_3(4, 3)).

% Updates a piece variable
update_piece(InitLine,InitCol,DestLine,DestCol,Player):-
    (Player = 1,
        ( 
            (p1_1(A,B), A = InitLine,B = InitCol, retract(p1_1(A,B)), assert(p1_1(DestLine,DestCol)));
            (p1_2(A,B), A = InitLine,B = InitCol, retract(p1_2(A,B)), assert(p1_2(DestLine,DestCol)));
            (p1_3(A,B), A = InitLine,B = InitCol, retract(p1_3(A,B)), assert(p1_3(DestLine,DestCol)))
        )
    );
    (Player = 2,
        (
            (p2_1(A,B), A = InitLine,B = InitCol, retract(p2_1(A,B)), assert(p2_1(DestLine,DestCol)));
            (p2_2(A,B), A = InitLine,B = InitCol, retract(p2_2(A,B)), assert(p2_2(DestLine,DestCol)));
            (p2_3(A,B), A = InitLine,B = InitCol, retract(p2_3(A,B)), assert(p2_3(DestLine,DestCol)))
        )
    ).

% Sets a piece on the board list
set_piece(1, 1, [[_El|Rest1]|Rest2], [[Piece|Rest1]|Rest2], Piece).

set_piece(1, N, [[Elem|Rest1]|Rest2], [[Elem|Head]|Rest2], Piece):- 
	Next is N-1,
	set_piece(1, Next, [Rest1|Rest2], [Head|Rest2], Piece).

set_piece(N, NCol, [Elem |Rest1], [Elem|Out], Piece):- 
    Next is N-1,
	set_piece(Next, NCol, Rest1, Out, Piece).

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
    set_piece(InitLine, InitCol, Board, TempBoard, empty),
    set_piece(DestLine, DestCol, TempBoard, NewBoard, Piece),
    update_piece(InitLine,InitCol,DestLine,DestCol, Player).

% Checks if a move is duplicate, meaning that its useless in practice since the piece doesn't change places
is_duplicate([InitLine,InitCol,DestLine,DestCol]):-
    InitLine = DestLine, 
    InitCol = DestCol.

% Discards duplicate moves
discard_duplicate_moves([], NewList, NewList).

discard_duplicate_moves([Head | Tail], TempList, NewList):-
        (is_duplicate(Head),discard_duplicate_moves(Tail, TempList, NewList));
        (discard_duplicate_moves(Tail, [Head | TempList], NewList)).

% Generates a list of valid moves for each piece of the current player
valid_moves_piece(_Board,[],ListOfMoves,ListOfMoves).

valid_moves_piece(Board, [Head|Tail],List, ListOfMoves):-
    Init = Head,
    Curr = Head,
    valid_horizontal(Board, Curr, Init, [], HorMoves, -1),
    valid_vertical(Board, Curr, Init, HorMoves, HorVertMoves, -1),
    valid_diagonal(Board, Curr, Init, HorVertMoves, AllMoves, -1, -1),
    discard_duplicate_moves(AllMoves, [], NewAllMoves),
    valid_moves_piece(Board,Tail, [NewAllMoves | List], ListOfMoves).

get_piece(LineN,ColN,Board,Piece):-
    nth1(LineN,Board,Line),
    nth1(ColN,Line,Piece).

/**
 * Returns the player's pieces positions on a list.
 */
get_pieces(Player, Pieces) :-
    (   Player=1,
        get_black_pieces(Pieces)
    ;   Player=2,
        get_white_pieces(Pieces)
    ).

/**
 * Returns the black pieces positions on a list.
 */ 
get_black_pieces(Pieces):-
    p1_1(A,B),
    p1_2(C,D),
    p1_3(E,F),
    Pieces = [[A,B],[C,D],[E,F]].
    
/**
 * Returns the white pieces positions on a list.
 */ 
get_white_pieces(Pieces):-
    p2_1(A,B),
    p2_2(C,D),
    p2_3(E,F),
    Pieces = [[A,B],[C,D],[E,F]].

% Generates a list of the valid moves for a piece, in its line
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
    get_piece(Line,NextCol,Board,Piece),
    /*else if*/((Piece = black ; Piece = white), 
                Move = [InitLine, InitCol,Line,Col], 
                NextInc is Inc + 2,
                Next_Col is InitCol,
                valid_horizontal(Board, [Line,Next_Col] , [InitLine,InitCol] , [Move | List] , Moves, NextInc)
               );
    /*else*/(valid_horizontal(Board, [Line,NextCol], [InitLine,InitCol] , List, Moves , Inc))
    ).    

% Generates a list of the valid moves for a piece, in its column
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
    get_piece(NextLine,Col,Board,Piece),
    /*else if*/((Piece = black ; Piece = white),
                Move = [InitLine, InitCol,Line,Col], 
                NextInc is Inc + 2, 
                Next_Line is InitLine,
                valid_vertical(Board, [Next_Line,Col] , [InitLine,InitCol] , [Move | List], Moves , NextInc)
               );
    /*else*/(valid_vertical(Board, [NextLine,Col], [InitLine,InitCol] , List, Moves , Inc))
    ).

% Generates a list of the valid moves for a piece, in its diagonal
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
    get_piece(NextLine,NextCol,Board,Piece),
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

% Generates a list of valid moves for a given player
valid_moves(Board, Player, ListOfMoves):-
    get_pieces(Player, Pieces),
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

/*
 Checks if the same board configuration has happened 3 times (three-fold repetition),
 in which case a draw occurs.
 */
game_over_draw(Winner):-
    countOcorrences(Count),
    member(3, Count),
    Winner = -1.

game_over_draw(Winner):-
    Winner = 0.

/**
 * Checks if a player has three consecutive pieces in a same row, thus winning the game.
 */
game_over_row(Winner) :-
    get_white_pieces(Pieces),
    are_consecutive_hor(Pieces),
    Winner=2.

game_over_row(Winner) :-
    get_black_pieces(Pieces),
    are_consecutive_hor(Pieces),
    Winner=1.

/**
 * Checks if a player has three consecutive pieces in a same diagonal, thus winning the game.
 */
game_over_diag(Winner) :-
    get_black_pieces(Pieces),
    are_consecutive_diag(Pieces),
    Winner=1.

game_over_diag(Winner) :-
    get_white_pieces(Pieces),
    are_consecutive_diag(Pieces),
    Winner=2.

/**
 * Checks if a player has three consecutive pieces in a same column, thus winning the game.
 */
game_over_col(Winner) :-
    get_black_pieces(Pieces),
    are_consecutive_ver(Pieces),
    Winner=1.

game_over_col(Winner) :-
    get_white_pieces(Pieces),
    are_consecutive_ver(Pieces),
    Winner=1.

/**
 * Checks if three given numbers are consecutive.
 */ 
are_numbers_consecutive(N1, N2, N3) :-
    Min1 is min(N2, N3),
    Min2 is min(N1, Min1),
    Max1 is max(N2, N3),
    Max2 is max(N1, Max1),
    Res is Max2-Min2,
    Res=2.

/**
 * Checks if three given pieces are consecutive in a board line.
 */ 
are_consecutive_hor(Pieces) :-
    nth0(0, Pieces, [F1|F2]),
    nth0(1, Pieces, [S1|S2]),
    nth0(2, Pieces, [T1|T2]),
    F1=S1,
    S1=T1,
    are_numbers_consecutive(F2, S2, T2).

/**
 * Checks if three given pieces are consecutive in a board column.
 */
are_consecutive_ver(Pieces) :-
    nth0(0, Pieces, [F1|F2]),
    nth0(1, Pieces, [S1|S2]),
    nth0(2, Pieces, [T1|T2]),
    F2=S2,
    S2=T2,
    are_numbers_consecutive(F1, S1, T1).

/**
 * Checks if three given pieces are consecutive in a board diagonal.
 */
are_consecutive_diag(Pieces) :-
    nth0(0, Pieces, [F1|F2]),
    nth0(1, Pieces, [S1|S2]),
    nth0(2, Pieces, [T1|T2]),
    are_numbers_consecutive(F1, S1, T1),
    are_numbers_consecutive(F2, S2, T2).


is_empty(Piece) :-
    Piece=empty.

is_black(Player, Piece) :-
    Player=1,
    Piece=black.

is_white(Player, Piece) :-
    Player=2,
    Piece=white.

% Gets a player move, not allowing absurd options
choose_player_move(ListOfMoves,Move):- 
    write('\nMove?'),
    read(Option),
    (get_move(Option,ListOfMoves, Move) -> true ; write('Please choose a valid option.\n'), choose_player_move(ListOfMoves,Move)).

get_move_piece(1, [Head | _Tail], _Pieces, Head).

get_move_piece(Option,[],[_Piece | Rest], Move):-
    get_move(Option, Rest , Move).
    
get_move_piece(Option,[_Head | Tail], [Piece | Rest], Move):-
    NextOption is Option - 1,
    get_move_piece(NextOption, Tail, [Piece | Rest] , Move).

get_move(Option, [Head | Tail], Move):-
    get_move_piece(Option, Head, [Head | Tail], Move).

/*
    Updates draw related variables, at the end of each game turn. If the current board is new,
    then it is appended to the board's list and a new element is added to the count list (1).
    Else, the boards lists remains unaltered and the corresponding count is incremented.
*/
handle_draw(NewBoard, Boards, CountOcurrences) :-
    (member(NewBoard, Boards),
        (
            nth0(Index, Boards, NewBoard),
            nth0(Index, CountOcurrences, Count),
            NewCount is Count+1,
            replace(CountOcurrences, Index, NewCount, NewCountOcurrences),
            retract(countOcorrences(CountOcurrences)),
            assert(countOcorrences(NewCountOcurrences))
        )
    );   
    (
        append(Boards, [NewBoard], TempNewBoards),
        append(CountOcurrences, [1], TempNewCount),
        retract(boards(Boards)),
        retract(countOcorrences(CountOcurrences)),
        assert(boards(TempNewBoards)),
        assert(countOcorrences(TempNewCount))
    ).