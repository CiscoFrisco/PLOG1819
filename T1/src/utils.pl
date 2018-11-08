replace([H|T], I, X, [H|R]):- 
    I > 0, 
    I1 is I-1, 
    replace(T, I1, X, R).

get_char_index(Col,Char):-
    TempCol is Col + 64,
    char_code(Char,TempCol).

not(X):- X, !, fail.
not(_X).