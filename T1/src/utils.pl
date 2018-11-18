/**
 * Replaces and element of a list for another, returning the resulting list.
 * 
 * replace(+List, +Index, +NewElement, -NewList)
 */ 
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- 
    I > 0, 
    I1 is I-1, 
    replace(T, I1, X, R).
 
/**
 * Converts integer index to letter. Useful to show the valid moves,
 * since the column index is shown as a letter.
 *
 * get_char_index(+Col, -Char)
 */  
get_char_index(Col,Char):-
    TempCol is Col + 64,
    char_code(Char,TempCol).

/**
 * Not operator (Most commonly (!) in other languages)
 * 
 * not(+X)
 */  
not(X):- X, !, fail.
not(_X).

/**
 * Sets a variable to another value.
 * 
 * set(+X, -X)
 */ 
set(X, X).


/**
 * duplicate(+List)
 */ 
duplicate(List):-
    append(X,Y,List),
    member(M,X),
    member(M,Y). 

/**
 * True if any given numbers in a list are consecutive.
 *
 * are_numbers_consecutive(+Numbers)
 */ 
are_numbers_consecutive(Numbers) :-
    not(duplicate(Numbers)),
    max_member(Max, Numbers),
    min_member(Min, Numbers),
    Res is Max - Min,
    length(Numbers, Length),
    Res =:= Length - 1.

/**
 * If-then-else construct in prolog
 * 
 * if_then_else(+Condition, +Goal, +Else)
 */ 
if_then_else(Condition, Goal, _Else) :- Condition, !, Goal.
if_then_else(_Condition, _Goal, Else) :- Else.

/**
 * Sorts a list of pairs by the x (or i) coordinate
 * 
 * sort_by_x(+List, -Sorted)
 */ 
sort_by_x([X|Xs],Ys):-
    partition(Xs,X,Littles,Bigs),
    sort_by_x(Littles,Ls),
    sort_by_x(Bigs,Bs),
    append(Ls,[X | Bs],Ys).

sort_by_x([],[]).

/**
 * partition(+List1,+List2, -List3, -List4)
 */ 
partition([[XX,XY]|Xs],[YX,YY],[[XX,XY]|Ls],Bs):- XY =< YY, partition(Xs,[YX,YY],Ls,Bs).
partition([[XX,XY]|Xs],[YX,YY],Ls,[[XX,XY]|Bs]):- XY > YY, partition(Xs,[YX,YY],Ls,Bs).
partition([],_Y,[],[]).
