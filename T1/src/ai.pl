choose_move(Board, 2, NextBoard):-
    nextPlayer(Player),
     alphabeta([Player,play,Board], -10000, 10000, [_, _, NextBoard], _, 1),
     update_pieces_ai(Player, NextBoard).

choose_move(Board, 1, NextBoard):-
    nextPlayer(Player),
    moves([Player, play, Board], PosList),
    length(PosList, Length),
    random(0,Length,Random),
    write(Random), nl,
    nth0(Random, PosList, [_, _, NextBoard]),
    update_pieces_ai(Player, NextBoard).
    
update_pieces_ai(Player, Board):-
    player_piece(Player, Piece),
    get_pieces(Board, Piece, Pieces),
    update_pieces(Player, Pieces).

update_pieces(1, [[I1, J1], [I2, J2], [I3, J3]]):-
    p1_1(A,B), retract(p1_1(A,B)), assert(p1_1(I1, J1)),
    p1_2(C,D), retract(p1_2(C,D)), assert(p1_2(I2, J2)),
    p1_3(E,F), retract(p1_3(E,F)), assert(p1_3(I3, J3)).

update_pieces(2, [[I1, J1], [I2, J2], [I3, J3]]):-
    p2_1(A,B), retract(p2_1(A,B)), assert(p2_1(I1, J1)),
    p2_2(C,D), retract(p2_2(C,D)), assert(p2_2(I2, J2)),
    p2_3(E,F), retract(p2_3(E,F)), assert(p2_3(I3, J3)).

% nextPlayer(X1, X2)
% True if X2 is the next player to play after X1.
nextPlayer(1, 2).
nextPlayer(2, 1).

game_over_ai(Board, Winner):-
    game_over_ai_row(Board, Winner).

game_over_ai(Board, Winner):-
    game_over_ai_col(Board, Winner).

game_over_ai(Board, Winner):-
    game_over_ai_diag(Board, Winner).

game_over_ai(Board, Winner):-
    game_over_ai_draw(Board, Winner).

game_over_ai_row(Board, 1):-
    get_pieces(Board, black, Pieces),
    are_consecutive_hor(Pieces).

game_over_ai_row(Board, 2):-
    get_pieces(Board, white, Pieces),
    are_consecutive_hor(Pieces).

game_over_ai_col(Board, 1):-
    get_pieces(Board, black, Pieces),
    are_consecutive_ver(Pieces).

game_over_ai_col(Board, 2):-
    get_pieces(Board, white, Pieces),
    are_consecutive_ver(Pieces).

game_over_ai_diag(Board, 1):-
    get_pieces(Board, black, Pieces),
    are_consecutive_diag(Pieces).

game_over_ai_diag(Board, 2):-
    get_pieces(Board, white, Pieces),
    are_consecutive_diag(Pieces).

game_over_ai_draw(Board, -1):-
    countOccurrences(CountOccurrences),
    boards(Boards),
    member(Board, Boards),
    nth0(Index, Boards, Board),
    nth0(Index, CountOccurrences, 2).

game_over_ai_draw(_Board, 0).

are_consecutive_ai(Pieces):-
    are_consecutive_ai_hor(Pieces).

are_consecutive_ai(Pieces):-
    are_consecutive_ai_ver(Pieces).

are_consecutive_ai(Pieces):-
    are_consecutive_ai_diag(Pieces).

are_consecutive_ai_hor([[F1,F2], [S1,S2]]):-
    F1 = S1,
    are_numbers_consecutive([F2, S2]).

are_consecutive_ai_ver([[F1,F2], [S1,S2]]):-
    F2 = S2,
    are_numbers_consecutive([F1, S1]).

are_consecutive_ai_diag([[F1,F2], [S1,S2]]):-
    are_numbers_consecutive([F1, S1]),
    are_numbers_consecutive([F2, S2]).
    
% get_pieces_line(Line, NLine, NCol, Type (black/white), Pieces, NewPieces)

get_pieces_line([], _NLine, _NCol, _Type, NewPieces, NewPieces).

get_pieces_line([Head | Rest], NLine, NCol, Head, Pieces, NewPieces):-
    append(Pieces, [[NLine, NCol]], TempPieces),
    NewNCol is NCol + 1,
    get_pieces_line(Rest, NLine,NewNCol, Head,TempPieces, NewPieces).

get_pieces_line([_Head | Rest], NLine, NCol, Type, Pieces, NewPieces):-
    NewNCol is NCol + 1,
    get_pieces_line(Rest, NLine,NewNCol, Type, Pieces, NewPieces).

get_pieces_aux([], _NLine, _NCol, _Type, NewPieces, NewPieces).

get_pieces_aux([Head | Rest], NLine, NCol, Type, Pieces, NewPieces):-
    get_pieces_line(Head, NLine, NCol, Type, Pieces, TempPieces),
    NewNLine is NLine + 1,
    get_pieces_aux(Rest,NewNLine, 1, Type, TempPieces, NewPieces).

% get_pieces(Board, white/black, -Pieces)
get_pieces(Board, Type, Pieces):-
    get_pieces_aux(Board, 1, 1, Type, [], Pieces).

value([1,win,_], 100).
value([_,draw,_], 0).
value([2,win,_], -100).

% Player 1 has advantage (2 consecutive pieces)
value([_, play, Board], 10):-
    get_pieces(Board, black, [[F1,F2], [S1,S2], [T1,T2]]),
    are_consecutive_ai([[F1,F2], [S1,S2]]);
    are_consecutive_ai([[F1,F2], [T1,T2]]);
    are_consecutive_ai([[S1,S2], [T1,T2]]), not(value([_, play, Board], -10)).

% Player 2 has advantage (2 consecutive pieces)
value([_, play, Board], -10):-
    get_pieces(Board, white, [[F1,F2], [S1,S2], [T1,T2]]),
    are_consecutive_ai([[F1,F2], [S1,S2]]);
    are_consecutive_ai([[F1,F2], [T1,T2]]);
    are_consecutive_ai([[S1,S2], [T1,T2]]), not(value([_, play, Board], 10)).

% No player has advantage
value([_, play, _Board], 0).

min_to_move([1,_, _]).

max_to_move([2,_,_]).

player_piece(1, black).
player_piece(2, white).

% pos [JOGADOR, ESTADO (win/draw/play), TABULEIRO]
% gerar lista com todas as posicoes validas a partir de pos (verificar final de jogo, e nesse caso dar fail)
moves([Player, _State, Board], PosList):-
    player_piece(Player,Type),
    get_pieces(Board, Type, Pieces),
    valid_moves_piece(Board, Pieces,[],Moves),
    generate_pos_list([Player, play, Board], Moves, _TempPosList, PosList).

generate_pos_list([_Player, _State, _Board], [], PosList, PosList).

generate_pos_list([Player, State, Board], [Head | Tail], TempPosList,PosList):-
    move_ai([Player, State, Board], Head, NewPos),
    append(TempPosList, [NewPos], NewPosList),
    generate_pos_list([Player, State, Board], Tail, NewPosList,PosList).


% Current Player wins with this move
move_ai([Player, play, Board], Move, [NextPlayer, win, NewBoard]):-
    nextPlayer(Player, NextPlayer),
    move_ai_aux(Move, Board, NewBoard,Player),
    game_over_ai(NewBoard, Player), !.

% Game ends in draw with this move
move_ai([Player, play, Board], Move, [NextPlayer, draw, NewBoard]):-
    nextPlayer(Player, NextPlayer),
    move_ai_aux(Move, Board, NewBoard,Player),
    game_over_ai(NewBoard, -1), !.

% Game ends in draw with this move
move_ai([Player, play, Board], Move, [NextPlayer, play, NewBoard]):-
    nextPlayer(Player, NextPlayer),
    move_ai_aux(Move, Board, NewBoard, Player).

move_ai_aux([InitLine, InitCol, DestLine, DestCol], Board, NewBoard, Player) :-
    player_piece(Player,Piece),
    set_piece(InitLine, InitCol, Board, TempBoard, empty),
    set_piece(DestLine, DestCol, TempBoard, NewBoard, Piece).

% Generic alpha beta algorithm from Bratko's Prolog programming for artificial intelligence
alphabeta(Pos, Alpha, Beta, GoodPos, Val, Depth) :-
    Depth > 0,
    moves(Pos, PosList),!,
    boundedbest(PosList, Alpha, Beta, GoodPos, Val, Depth);
    value(Pos, Val).

boundedbest([Pos | PosList], Alpha, Beta, GoodPos, GoodVal, Depth) :-
    NextDepth is Depth - 1, 
    alphabeta(Pos, Alpha, Beta, _, Val, NextDepth),
    goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal, Depth).

goodenough([],_,_, Pos, Val, Pos, Val, _) :- !.

goodenough(_, Alpha, Beta, Pos, Val, Pos, Val, _) :-
    min_to_move(Pos), Val > Beta,!; % Maximizer attained upper bound
    max_to_move(Pos), Val < Alpha,!.

goodenough(PosList, Alpha, Beta, Pos, Val, GoodPos, GoodVal, Depth) :-
    newbounds(Alpha, Beta, Pos, Val, NewAlpha, NewBeta), % Refine bounds
    boundedbest(PosList, NewAlpha, NewBeta, Pos1, Val1, Depth),
    betterof(Pos, Val, Pos1, Val1, GoodPos, GoodVal).

newbounds(Alpha, Beta, Pos, Val, Val, Beta) :-
    min_to_move(Pos), Val > Alpha,!.

newbounds(Alpha, Beta, Pos, Val, Alpha, Val) :-
    max_to_move(Pos), Val < Beta,!.

newbounds(Alpha, Beta, _, _, Alpha, Beta).

betterof(Pos, Val, _, Val1, Pos, Val) :- % Pos better than Posl
    min_to_move(Pos), Val > Val1, !;
    max_to_move(Pos), Val < Val1, !.

betterof( _, _, Pos1, Val1, Pos1, Val1).