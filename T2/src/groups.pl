:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(file_systems)).

:- consult('io.pl').

/**
 * groups(+Students, +Proj1Themes, +Proj2Themes, +GroupSize, +PreviousUCsInfo, -Proj1Groups, -Proj2Groups)
 * 
 * Students - list of students (name/id and their gpa)
 * NumThemes1 - number of available themes for the first project
 * NumThemes2 - number of available themes for the second project
 * GroupSize - interval representing the possible group sizes
 * PreviousUCsInfo - list of students that have worked together before
 * Proj1Groups - list of groups for the first project
 * Proj2Groups - list of groups for the second project
 */ 
groups(Students, [MinSize , MaxSize], PreviousUCsInfo, Proj1Themes, Proj2Themes, Proj1Groups, Proj2Groups).

/**
 * main(+CWD, +StudentsFile, +PreviousUCsInfoFile, +GroupSize)
 * 
 * CWD - current working directory
 * StudentsFile - students file path relative to cwd
 * NumThemes1 - number of available themes for the first project
 * NumThemes2 - number of available themes for the second project
 * PreviousUCsInfoFile - previousUCsInfo file path relative to cwd
 * GroupSize - interval representing the possible group sizes
 */ 
main(CWD, StudentsFile, PreviousUCsInfoFile, Proj1ThemesFile, Proj2ThemesFile, GroupSize):-
    current_directory(_, CWD),
    read_files(StudentsFile, PreviousUCsInfoFile, Proj1ThemesFile, Proj2ThemesFile, Students, PreviousUCsInfo, Proj1Themes, Proj2Themes),
    groups(Students, GroupSize, PreviousUCsInfo, Proj1Themes, Proj2Themes, Proj1Groups, Proj2Groups),
    write_files(Proj1Groups, Proj2Groups).

/**
 * haveWorkedTogether(+Student1, +Student2, +ListOfGroups)
 * 
 * True if Student1 and Student2 have worked together before according to the given information.
 * 
 * Student1 - id of student1
 * Student2 - id of student2
 * ListOfGroups - list of students that have worked together before (in other UCs or in the first project)
 */  
haveWorkedTogether(_, _, [], 1):-write('aqui'),nl.
haveWorkedTogether(Student1, Student2, [H | _T], 0):-
    member(Student1, H),
    write('s1:'),write(Student1),nl,
    member(Student2, H),
    write('s2:'),write(Student2),nl.
    
haveWorkedTogether(Student1, Student2, [_H | T], Res):-
    haveWorkedTogether(Student1, Student2, T, Res).

% TESTES

constrain_group_size(Students, PreviousUCsInfo, [MinSize, MaxSize], Vars):-
    length(Students, NumStudents),
    length(Vars, NumStudents),
    MaxNumGroups is NumStudents div MinSize,
    MinNumGroupsMod is NumStudents mod MaxSize,
    if_then_else((MinNumGroupsMod = 0),(MinNumGroups is NumStudents div MaxSize),(MinNumGroups is (NumStudents div MaxSize) + 1)),
    domain([Max],MinNumGroups,MaxNumGroups),
    domain(Vars,1,MaxNumGroups),
    % max_member(MaxNum,Vars),
    % MaxNum #= Max,
    constrain_count(Vars,[MinSize,MaxSize], Max, 1),
    constrain_worked_before(Students, PreviousUCsInfo, Vars, Max, 1),
    labeling([], [Max | Vars]), 
    write(Vars),nl.

constrain_count(_Vars,_,Max,Num):- Num #> Max,!.
constrain_count(Vars, [MinSize,MaxSize], Max,Num):-
    count(Num, Vars, #=, Times),
    Times #>= MinSize #/\ Times #=< MaxSize,
    NextNum is Num + 1,
    constrain_count(Vars,[MinSize,MaxSize],Max, NextNum).

if_then_else(C,I,_E):-C,!,I.
if_then_else(_C,_I,E):- E.

constrain_worked_before_aux(_, _, []).
constrain_worked_before_aux(Students, PreviousUCsInfo, [[S1, S2] | T]):-
    nth1(S1, Students, Student1),
    nth1(S2, Students, Student2),
    haveWorkedTogether(Student1, Student2, PreviousUCsInfo, Res),
    Res #= 1,
    format("~d - ~d~n", [Student1, Student2]),
    write(Res),nl,
    constrain_worked_before_aux(Students, PreviousUCsInfo, T).

constrain_worked_before(_, _, _,Max, Num):- Num #> Max, !.
constrain_worked_before(Students, PreviousUCsInfo, Vars, Max, Num):-
    write('\nYee '), write(Num),nl,
    findall(X, nth1(X, Vars, Num), GroupElems),
    length(GroupElems,GroupLen),
    if_then_else(
                    (GroupLen \= 1),
                    (
                        findall(Y, comb(2,GroupElems,Y), StudentPairs),
                        constrain_worked_before_aux(Students, PreviousUCsInfo, StudentPairs),
                        NextNum is Num + 1,
                        constrain_worked_before(Students,PreviousUCsInfo, Vars, Max, NextNum)
                    ),
                    _
                ).

comb(0,_,[]).
comb(N,[X|T],[X|Comb]):-N>0,N1 is N-1,comb(N1,T,Comb).
comb(N,[_|T],Comb):-N>0,comb(N,T,Comb).