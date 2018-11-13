% Replaces and element of a list for another, returning the resulting list
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- 
    I > 0, 
    I1 is I-1, 
    replace(T, I1, X, R).

% Converts letter to integer index
get_char_index(Col,Char):-
    TempCol is Col + 64,
    char_code(Char,TempCol).

% Not operator (Most commonly (!) in other languages)
not(X):- X, !, fail.
not(_X).

% Sets a variable to another value.
set(X, X).

/**
 * True if any given numbers in a list are consecutive.
 */ 
are_numbers_consecutive(Numbers) :-
    max_member(Max, Numbers),
    min_member(Min, Numbers),
    Res is Max - Min,
    length(Numbers, Length),
    Res =:= Length - 1.
