choose_move(Board, Level, NextBoard):-
    nextPlayer(Player),
     alphabeta([Player,play,Board], -1000, 1000, [_, _, NextBoard], _, Level),
     update_piece_ai(Player, NextBoard, 1, 1).

update_piece_ai(Player, [Line | Rest], NLine, NCol):-
    update_piece_ai_aux(Player, Line, NLine, NCol),
    NewNCol is NCol + 1,
    update_piece_ai(Player, Rest, 1, NewNCol).

update_piece_ai_aux(_Player, [] , _NLine, _NCol).

update_piece_ai_aux(Player, [Piece | Rest], NLine, NCol):-
    NewNLine is NLine + 1,
    (   (Piece = black, Player = 1,
            ((
                (p1_1(A,B), (A \= NLine;B \= NCol), retract(p1_1(A,B)), assert(p1_1(NLine,NCol)));
                (p1_2(A,B), (A \= NLine;B \= NCol), retract(p1_2(A,B)), assert(p1_2(NLine,NCol)));
                (p1_3(A,B), (A \= NLine;B \= NCol), retract(p1_3(A,B)), assert(p1_3(NLine,NCol)))
            ) 
            ;
            update_piece_ai_aux(Player, Rest, NewNLine, NCol))
        );

        (Piece = white, Player = 2,
            ((
                (p2_1(A,B), (A \= NLine;B \= NCol), retract(p2_1(A,B)), assert(p2_1(NLine,NCol)));
                (p2_2(A,B), (A \= NLine;B \= NCol), retract(p2_2(A,B)), assert(p2_2(NLine,NCol)));
                (p2_3(A,B), (A \= NLine;B \= NCol), retract(p2_3(A,B)), assert(p2_3(NLine,NCol)))
            )
            ;
            update_piece_ai_aux(Player, Rest, NewNLine, NCol))   
        ) 
    ).


% Generic alpha beta algorithm from Bratko's Prolog programming for artificial intelligence
alphabeta(Pos, Alpha, Beta, GoodPos, Val, Depth) :-
    moves(Pos, PosList),!,
    boundedbest(PosList, Alpha, Beta, GoodPos, Val, Depth);
    value(Pos, Val).

% pos [JOGADOR, ESTADO (win/draw/play), TABULEIRO]
% gerar lista com todas as posicoes validas a partir de pos (verificar final de jogo, e nesse caso dar fail)
moves(Pos, PosList):-
    bagof(NextPos,move_ai(Pos, NextPos),PosList).

% nextPlayer(X1, X2)
% True if X2 is the next player to play after X1.
nextPlayer(1, 2).
nextPlayer(2, 1).

% move_ai(+Pos, -NextPos)
% True if there is a legal (according to rules) move_ai from Pos to NextPos.
move_ai([P1, play, Board], [P2, win, NextBoard]) :-
    nextPlayer(P1, P2),
    move_aux(P1, Board, NextBoard),
    winPos(P1, NextBoard), !.

move_ai([P1, play, Board], [P2, draw, NextBoard]) :-
    nextPlayer(P1, P2),
    move_aux(P1, Board, NextBoard),
    drawPos(P1,NextBoard), !.

move_ai([P1, play, Board], [P2, play, NextBoard]) :-
    nextPlayer(P1, P2),
    move_aux(P1, Board, NextBoard).

% move_aux(+Player, +Board, -NextBoard)
% True if NextBoard is Board whith an empty case replaced by Player mark.
move_aux(P, [0|Bs], [P|Bs]).

move_aux(P, [B|Bs], [B|B2s]) :-
    move_aux(P, Bs, B2s).

% winPos(+Player, +Board)
% True if Player win in Board.
winPos(P, Board) :-
    game_over_ai(Board, P).

game_over_ai(Board, Winner):-
    game_over_ai_row(Board, Winner).

game_over_ai(Board, Winner):-
    game_over_ai_col(Board, Winner).

game_over_ai(Board, Winner):-
    game_over_ai_diag(Board, Winner).

game_over_ai(Board, Winner):-
    game_over_ai_draw(Board, Winner).

game_over_ai_row(Board, 1):-
    get_black_pieces(Board, Pieces),
    areConsecutiveHor(Pieces).

game_over_ai_row(Board, 2):-
    get_white_pieces(Board, 2, Pieces),
    areConsecutiveHor(Pieces).

game_over_ai_col(Board, 1):-
    get_black_pieces(Board, Pieces),
    areConsecutiveVer(Pieces).

game_over_ai_col(Board, 2):-
    get_white_pieces(Board, 2, Pieces),
    areConsecutiveVer(Pieces).

game_over_ai_diag(Board, 1):-
    get_black_pieces(Board, Pieces),
    areConsecutiveDiag(Pieces).

game_over_ai_diag(Board, 2):-
    get_white_pieces(Board, 2, Pieces),
    areConsecutiveDiag(Pieces).

game_over_ai_draw(Board, -1):-
    countOcorrences(CountOcorrences),
    boards(Boards),
    member(Board, Boards),
    nth0(Index, Boards, Board),
    nth0(Index, CountOcorrences, 2).

game_over_ai_draw(_Board, 0).

% get_white_pieces_line(Board, Pieces, NLine).

% get_white_pieces(Board, Pieces):-
%     get_white_pieces_line(Board, Pieces, NLine).


    
% drawPos(+Player, +Board)
% True if the game is a draw.
drawPos(_,Board) :-
    game_over(Board, -1).

value([1,win,_], 100).
value([_,draw,_], 0).
value([2,win,_], -100).

% Player 1 has advantage (2 consecutive pieces)
value([1, play, Board], 10):-
    get_black_pieces(Board, [[F1|F2], [S1|S2], [T1|T2]]),
    areConsecutive([[F1|F2], [S1|S2]]);
    areConsecutive([[F1|F2], [T1|T2]]);
    areConsecutive([[S1|S2], [T1|T2]]).

% Player 2 has advantage (2 consecutive pieces)
value([2, play, Board], -10):-
    get_white_pieces(Board, [[F1|F2], [S1|S2], [T1|T2]]),
    areConsecutive([[F1|F2], [S1|S2]]);
    areConsecutive([[F1|F2], [T1|T2]]);
    areConsecutive([[S1|S2], [T1|T2]]).

% No player has advantage
value([_, play, _Board], 0).

min_to_move([1,_, _]).

max_to_move([2,_,_]).

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

betterof(Pos, Val, _Pos1, Val1, Pos, Val) :- % Pos better than Posl
    min_to_move(Pos), Val > Val1, !;
    max_to_move(Pos), Val < Val1,!.

betterof( _, _, Pos1, Val1, Pos1, Val1).