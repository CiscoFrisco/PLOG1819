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
haveWorkedTogether(_, _, [], 0):-fail.
haveWorkedTogether(Student1, Student2, [H | _], 1):-
    member(Student1, H),
    member(Student2, H).
haveWorkedTogether(Student1, Student2, [_ | T],_):-
    haveWorkedTogether(Student1, Student2, T).

% TESTES

constrain_group_size(Students, [MinSize, MaxSize], Vars):-
    length(Students, NumStudents),
    length(Vars, NumStudents),
    MinNumGroups is NumStudents div MaxSize,
    MaxNumGroups is NumStudents div MinSize,
    domain([Max],MinNumGroups,MaxNumGroups),
    constrain_count(Vars,[MinSize,MaxSize], Max, 1),
    labeling([],[Max]),
    domain(Vars,1,Max),
    write('Max:'),write(Max),nl,
    labeling([], Vars),
    write(Vars),
    nl.

constrain_count(_,_,Max,Num):- Num #> Max,!. % tirando este cut funciona mas nunca mais acaba. Com este cut apenas tenta para um valor de Max
constrain_count(Vars, [MinSize,MaxSize], Max, Num):-
    count(Num, Vars, #=, Times),
    Times #>= MinSize #/\ Times #=< MaxSize,
    NextNum is Num + 1,
    constrain_count(Vars,[MinSize,MaxSize], Max, NextNum).

