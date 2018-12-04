read_files(StudentsFile, Proj1ThemesFile, Proj2ThemesFile, PreviousUCsInfoFile, Students, Proj1Themes, Proj2Themes, PreviousUCsInfo):-
    open(StudentsFile, read, Str),
    read_students(Str, Students),
    close(Str),
    open(Proj1ThemesFile, read, Str2),
    read_themes(Str2, Proj1Themes),
    close(Str2),
    open(Proj2ThemesFile, read, Str3),
    read_themes(Str3, Proj2Themes),
    close(Str3),
    open(PreviousUCsInfoFile, read, Str4),
    read_students(Str4, PreviousUCsInfo),
    close(Str4).

read_themes(Stream,[]) :-
    at_end_of_stream(Stream).

read_themes(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_themes(Stream,L).

read_students(Stream,[]) :-
    at_end_of_stream(Stream).

read_students(Stream,[[X, Y]|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read(Stream,Y),
    read_students(Stream,L).

write_files(Proj1Groups, Proj2Groups):-
    open('proj1groups.txt', write, Str),
    write_groups(Str, Proj1Groups),
    close(Str),
    open('proj2groups.txt', write, Str2),
    write_groups(Str2, Proj2Groups),
    close(Str2).

write_group(Stream, []):-write(Stream, '\n').

write_group(Stream, [H | T]):-
    write(Stream, H),
    write(Stream, '  '),
    write_group(Stream, T).

write_groups(_, []).
write_groups(Stream, [H | T]):-
    write_group(Stream, H),
    write_groups(Stream, T).