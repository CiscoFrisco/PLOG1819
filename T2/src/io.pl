read_files(StudentsFile, PreviousUCsInfoFile, Proj1ThemesFile, Proj2ThemesFile, Students, GPAs, PreviousUCsInfo, Proj1Themes, Proj2Themes):-
    open(StudentsFile, read, Str),
    read_students_file(Str, Students, GPAs),
    close(Str),
    open(PreviousUCsInfoFile, read, Str2),
    read_previous_ucs_file(Str2, PreviousUCsInfo),
    open(Proj1ThemesFile, read, Str3),
    read_themes_file(Str3, Proj1Themes),
    open(Proj2ThemesFile, read, Str4),
    read_themes_file(Str4, Proj2Themes),
    close(Str2), !.

read_themes_file(Stream,[]) :-
    at_end_of_stream(Stream), !.

read_themes_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_themes_file(Stream,L).

read_previous_ucs_file(Stream,[]) :-
    at_end_of_stream(Stream), !.

read_previous_ucs_file(Stream,[[X, Y]|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read(Stream,Y),
    read_previous_ucs_file(Stream,L).

read_students_file(Stream, [], []) :-
    at_end_of_stream(Stream), !.

read_students_file(Stream,[S|Students], [GPA | GPAs]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,S),
    read(Stream,GPA),
    read_students_file(Stream,Students, GPAs).

write_files(Proj1Groups, Proj2Groups, Proj1Themes, Proj2Themes):-
    open('proj1groups.txt', write, Str),
    length(Proj1Themes, Proj1ThemesLen),
    write(Str, 'PROJECT 1 GROUPS\n'),
    write('CARALHOOO\n'),
    write_groups(Str, Proj1Groups, 0, Proj1Themes, Proj1ThemesLen), 
    close(Str),
    open('proj2groups.txt', write, Str2),
    length(Proj2Themes, Proj2ThemesLen),
    write(Str2, 'PROJECT 2 GROUPS\n'),
    write_groups(Str2, Proj2Groups, 0, Proj2Themes, Proj2ThemesLen),
    close(Str2), !.

write_group(Stream, []):-write(Stream, '\n'), !.
write_group(Stream, [H | T]):-
    write(Stream, H),
    write(Stream, '  '),
    write_group(Stream, T).

write_groups(Stream, [], _, _, _) :- write(Stream, '\n'), !.
write_groups(Stream, [H | T], Count, Themes, ThemesLen):-
    GroupNum is Count + 1,
    format(Stream, 'Group ~d: ', [GroupNum]),
    write_group(Stream, H),
    Index is Count mod ThemesLen,
    nth0(Index, Themes, Theme),
    format(Stream, 'Theme: ~s~n~n', [Theme]),
    NextCount is Count + 1,
    write_groups(Stream, T, NextCount, Themes, ThemesLen). 

write_ter_group([]):- write('\n'), !.
write_ter_group([H | T]):-
    write(H),
    write('  '),
    write_ter_group(T).

write_ter([], _, _,_) :- write('\n'), !.
write_ter([H | T], Themes, ThemesLen, Count):-
    GroupNum is Count + 1,
    format('Group ~d: ', [GroupNum]),
    write_ter_group(H),
    NextCount is Count + 1,
    Index is Count mod ThemesLen,
    nth0(Index, Themes, Theme),
    format('Theme: ~s~n~n', [Theme]),
    write_ter(T, Themes, ThemesLen, NextCount).