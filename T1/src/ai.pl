:- dynamic(currPlayer/1).
:- dynamic(currMax/1).
:- dynamic(currMaxBoard/1).
:- dynamic(currRoot/1).
:- dynamic(lost/1).
:- dynamic(previousMax/1).
:- dynamic(previousMaxBoard/1).

/**
 * Useful data.
 */ 
currPlayer(0).
currMax(-2000).
currMaxBoard([]).
previousMax(-2000).
previousMaxBoard([]).
currRoot([]).
lost(0).

/**
 * player_piece(+Player, -Piece)
 */ 
player_piece(1, black).
player_piece(2, white).

/**
 * True if P2 is the next player to play after P1.
 * 
 * nextPlayer(+P1, +P2)
 */ 
nextPlayer(1, 2).
nextPlayer(2, 1).

/**
 * choose_move(+Board, +Depth, -NextBoard)
 */
choose_move(Board, Depth, NextBoard):-
    nextPlayer(Player),
    retract(currPlayer(_)),
    assert(currPlayer(Player)),
    retract(currMax(_)),
    assert(currMax(-2000)),
    retract(currMaxBoard(_)),
    assert(currMaxBoard([])),
    retract(previousMax(_)),
    assert(previousMax(-2000)),
    retract(previousMaxBoard(_)),
    assert(previousMaxBoard([])),
    retract(currRoot(_)),
    assert(currRoot([])),
    retract(lost(_)),
    assert(lost(0)),
    choose_move_aux([Player,play,Board], Depth),
    currMaxBoard(NextBoard),
    update_pieces_ai(Player, NextBoard).
    
/**
 * Update piece positions values after choosing the move.
 *     
 * update_pieces_ai(+Player, -Board)    
 */    
update_pieces_ai(Player, Board):-
    player_piece(Player, Piece),
    get_pieces(Board, Piece, Pieces),
    update_pieces_aux(Player, Pieces).

/**
 * Update piece positions values for Player 1.
 * 
 * update_pieces_aux(+Player, +Pieces)
 */ 
update_pieces_aux(1, [[I1, J1], [I2, J2], [I3, J3]]):-
    p1_1(A,B), retract(p1_1(A,B)), assert(p1_1(I1, J1)),
    p1_2(C,D), retract(p1_2(C,D)), assert(p1_2(I2, J2)),
    p1_3(E,F), retract(p1_3(E,F)), assert(p1_3(I3, J3)).

/**
 * Update piece positions values for Player 2.
 */ 
update_pieces_aux(2, [[I1, J1], [I2, J2], [I3, J3]]):-
    p2_1(A,B), retract(p2_1(A,B)), assert(p2_1(I1, J1)),
    p2_2(C,D), retract(p2_2(C,D)), assert(p2_2(I2, J2)),
    p2_3(E,F), retract(p2_3(E,F)), assert(p2_3(I3, J3)).

/**
 * Checks if in the current board, the game has ended.
 * This predicate is needed because the one already implemented makes use of
 * outside values to check the pieces positions.
 * 
 * game_over_ai(+Board, -Winner).
 */
game_over_ai(Board, Winner):-
    game_over_ai_row(Board, Winner).

game_over_ai(Board, Winner):-
    game_over_ai_col(Board, Winner).

game_over_ai(Board, Winner):-
    game_over_ai_diag(Board, Winner).

game_over_ai(Board, Winner):-
    game_over_ai_draw(Board, Winner).

/**
 * True if Player has 3 consecutive pieces horizontally.
 * 
 * game_over_ai_row(+Board, -Winner)
 */
game_over_ai_row(Board, 1):-
    get_pieces(Board, black, Pieces),
    are_consecutive_hor(Pieces).

game_over_ai_row(Board, 2):-
    get_pieces(Board, white, Pieces),
    are_consecutive_hor(Pieces).

/**
 * True if Player has 3 consecutive pieces vertically.
 * 
 * game_over_ai_col(+Board, -Winner)
 */
game_over_ai_col(Board, 1):-
    get_pieces(Board, black, Pieces),
    are_consecutive_ver(Pieces).

game_over_ai_col(Board, 2):-
    get_pieces(Board, white, Pieces),
    are_consecutive_ver(Pieces).

/**
 * True if Player has 3 consecutive pieces diagonally.
 * 
 * game_over_ai_diag(+Board, -Winner)
 */
game_over_ai_diag(Board, 1):-
    get_pieces(Board, black, Pieces),
    are_consecutive_diag(Pieces).

game_over_ai_diag(Board, 2):-
    get_pieces(Board, white, Pieces),
    are_consecutive_diag(Pieces).

/**
 * True if this Board has appeared 3 times in the current game.
 * 
 * game_over_ai_draw(+Board, -Winner)
 */
game_over_ai_draw(Board, -1):-
    countOccurrences(CountOccurrences),
    boards(Boards),
    member(Board, Boards),
    nth0(Index, Boards, Board),
    nth0(Index, CountOccurrences, 2).

game_over_ai_draw(_Board, 0).

/**
 * Checks if two pieces are consecutive.
 * 
 * are_consecutive_ai(+Pieces)
 */
are_consecutive_ai(Pieces):-
    are_consecutive_ai_hor(Pieces).

are_consecutive_ai(Pieces):-
    are_consecutive_ai_ver(Pieces).

are_consecutive_ai(Pieces):-
    are_consecutive_ai_diag(Pieces).

/**
 * Checks if two pieces are consecutive in the same line.
 * 
 * are_consecutive_ai_hor(+Pieces)
 */
are_consecutive_ai_hor([[F1,F2], [S1,S2]]):-
    F1 = S1,
    are_numbers_consecutive([F2, S2]).

/**
 * Checks if two pieces are consecutive in the same column.
 * 
 * are_consecutive_ai_ver(+Pieces)
 */
are_consecutive_ai_ver([[F1,F2], [S1,S2]]):-
    F2 = S2,
    are_numbers_consecutive([F1, S1]).

/**
 * Checks if two pieces are consecutive in the same diagonal.
 * 
 * are_consecutive_ai_diag(+Pieces)
 */ 
are_consecutive_ai_diag([[F1,F2], [S1,S2]]):-
    are_numbers_consecutive([F1, S1]),
    are_numbers_consecutive([F2, S2]).
    
/**
 * Return the pieces (i,j) in the current line, equal to the given Type.
 * 
 * get_pieces_line(+Line, +NLine, +NCol, +Type, -Pieces, -NewPieces)
 */ 
get_pieces_line([], _NLine, _NCol, _Type, NewPieces, NewPieces).

get_pieces_line([Head | Rest], NLine, NCol, Head, Pieces, NewPieces):-
    append(Pieces, [[NLine, NCol]], TempPieces),
    NewNCol is NCol + 1,
    get_pieces_line(Rest, NLine,NewNCol, Head,TempPieces, NewPieces).

get_pieces_line([_Head | Rest], NLine, NCol, Type, Pieces, NewPieces):-
    NewNCol is NCol + 1,
    get_pieces_line(Rest, NLine,NewNCol, Type, Pieces, NewPieces).

/**
 * Return the pieces (i,j) in the current Board equal to the given Type.
 * 
 * get_pieces_aux(+Board, +NLine, +NCol, +Type, -Pieces, -NewPieces)
 */ 
get_pieces_aux([], _NLine, _NCol, _Type, NewPieces, NewPieces).

get_pieces_aux([Head | Rest], NLine, NCol, Type, Pieces, NewPieces):-
    get_pieces_line(Head, NLine, NCol, Type, Pieces, TempPieces),
    NewNLine is NLine + 1,
    get_pieces_aux(Rest,NewNLine, 1, Type, TempPieces, NewPieces).

/**
 * Return the pieces (i,j) in the current Board equal to the given Type.
 * 
 * get_pieces(+Board, +Type, -Pieces)
 */ 
get_pieces(Board, Type, Pieces):-
    get_pieces_aux(Board, 1, 1, Type, [], Pieces).

/**
 * Evaluates the board according to the pieces positions.
 * 
 * value(+Board, -Value, +Depth).
 */ 
value(Board,Val, Depth):-
    currPlayer(P), 
    nextPlayer(P,NextP),
    (
        ( 
            check_win(P,Board,Depth,Val)
            ;
            (
                check_win(NextP,Board,Depth,ValL), 
                Val is 0 - ValL,
                retract(lost(_)),
                assert(lost(1))
            )
        )
        ;
        (   
            Depth =:= 1,
            value_aux(P,Board,Val1),
            value_aux(NextP,Board,Val2),
            Val is Val1 - Val2
        )
        ;
        Val is -2000
    ).

/**
 * Checks if the Player has won in this board, and returns the appropriate value
 * according to the tree Depth.
 * 
 * check_win(+Player, +Board, +Depth, -Val)
 */ 
check_win(Player,Board, Depth,Val):- 
    game_over_ai(Board,Player),
    Val is 100 * Depth.

/**
 * Player has advantage (2 consecutive pieces).
 * 
 * value_aux(+Player, +Board, -Value)
 */ 
value_aux(Player, Board, 10):-
    player_piece(Player,Piece),
    get_pieces(Board, Piece, [[F1,F2], [S1,S2], [T1,T2]]),
    are_consecutive_ai([[F1,F2], [S1,S2]]);
    are_consecutive_ai([[F1,F2], [T1,T2]]);
    are_consecutive_ai([[S1,S2], [T1,T2]]).

value_aux(_,_Board, 0).

/**
 * Generate list with all valid positions starting from Pos [Player, State, Board].
 * 
 * moves(+Pos, -PosList)
 */  
moves([Player, _State, Board], PosList):-
    player_piece(Player,Type),
    get_pieces(Board, Type, Pieces),
    valid_moves_piece(Board, Pieces,[],Moves),
    generate_pos_list([Player, play, Board], Moves, _TempPosList, PosList).

/**
 * Generate list with all valid positions starting from Pos.
 * 
 * generate_pos_list(+Pos, Moves, -TempPosList, -PosList)
 */ 
generate_pos_list([_Player, _State, _Board], [], PosList, PosList).

generate_pos_list([Player, State, Board], [Head | Tail], TempPosList,PosList):-
    move_ai([Player, State, Board], Head, NewPos),
    append(TempPosList, [NewPos], NewPosList),
    generate_pos_list([Player, State, Board], Tail, NewPosList,PosList).

/**
 * Current Player wins with this move.
 * 
 * move_ai(+Pos, +Move, -NextPos)
 */ 
move_ai([Player, play, Board], Move, [NextPlayer, win, NewBoard]):-
    nextPlayer(Player, NextPlayer),
    move_ai_aux(Move, Board, NewBoard,Player),
    game_over_ai(NewBoard, Player), !.

/**
 * Game ends in draw with this move.
 */ 
move_ai([Player, play, Board], Move, [NextPlayer, draw, NewBoard]):-
    nextPlayer(Player, NextPlayer),
    move_ai_aux(Move, Board, NewBoard,Player),
    game_over_ai(NewBoard, -1), !.

/**
 * Game continues with this move.
 */
move_ai([Player, play, Board], Move, [NextPlayer, play, NewBoard]):-
    nextPlayer(Player, NextPlayer),
    move_ai_aux(Move, Board, NewBoard, Player).

/**
 * move_ai_aux(+Move, +Board, -NewBoard, +Player)
 */ 
move_ai_aux([InitLine, InitCol, DestLine, DestCol], Board, NewBoard, Player) :-
    player_piece(Player,Piece),
    set_piece(InitLine, InitCol, Board, TempBoard, empty),
    set_piece(DestLine, DestCol, TempBoard, NewBoard, Piece).

/**
 * random_shuffle(+PosList, -List, -Moves)
 */ 
random_shuffle([],List,List).

random_shuffle(PosList,List,NewMoves):-
    length(PosList,L),
    random(0,L,Random),
    nth0(Random,PosList,Move),
    delete(PosList, Move,TempMoves),
    append(List,[Move], AnotherList),
    random_shuffle(TempMoves,AnotherList,NewMoves).

/**
 * choose_move_aux(+Pos, +Depth)
 */ 
choose_move_aux([_Player,_State,Board], Depth):-
    difficulty(Diff),
    Depth =:= Diff - 1,
    retract(currRoot(_)),
    assert(currRoot(Board)),
    difficulty(Diff),
    lost(L),
    (
        (
            L =:= 1,
            previousMax(PMax),
            previousMaxBoard(PMaxBoard),
            retract(lost(_)),
            retract(currMax(_)),
            retract(currMaxBoard(_)),
            assert(lost(0)),
            assert(currMax(PMax)),
            assert(currMaxBoard(PMaxBoard))
        )
        ;
        (   
            currMax(Max),
            currMaxBoard(MaxBoard),
            retract(previousMax(_)),
            retract(previousMaxBoard(_)),
            assert(previousMax(Max)),
            assert(previousMaxBoard(MaxBoard))
        )     
    ),
    fail.

choose_move_aux([_Player,_State,Board], Depth):-
    difficulty(Diff),
    Depth \= Diff,
    value(Board, Val, Depth),
    currMax(Max),
    currRoot(Root),
    (
        (
            Val > Max,
            retract(currMax(_)),
            retract(currMaxBoard(_)),
            assert(currMax(Val)),
            assert(currMaxBoard(Root))
        )
        ;
        true
    ),
    fail.

choose_move_aux([Player,State,Board], Depth) :-
    (
        Depth > 1,
        moves([Player,State,Board], PosList),!,
        random_shuffle(PosList,[],NewMoves),
        best(NewMoves, Depth)
    )
    ;
    true.

/**
 * best(+PosList, +Depth)
 */ 
best([],_Depth).

best([Pos | PosList], Depth):-
    NextDepth is Depth - 1, 
    choose_move_aux(Pos, NextDepth),
    best(PosList, Depth).