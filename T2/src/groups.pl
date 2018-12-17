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
groups(Students, [MinSize , MaxSize], PreviousUCsInfo, Proj1Themes, Proj2Themes, Proj1Vars, Proj2Vars, Max1, Max2).

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
    groups(Students, GroupSize, PreviousUCsInfo, Proj1Themes, Proj2Themes, Proj1Vars, Proj2Vars, Max1, Max2),
    get_groups(Students, Proj1Vars, [], Proj1Groups, 1, Max1),
    get_groups(Students, Proj2Vars, [], Proj2Groups, 1, Max2),
    write_files(Proj1Groups, Proj2Groups).

get_groups_aux(_, [], Group, Group).
get_groups_aux(Students, [H | T], CurrGroup, Group):-
    nth1(H, Students, Student),
    append(CurrGroup, [Student], NextGroup),
    get_groups_aux(Students, T, NextGroup, Group).

get_groups(_, _, _, Num, Max):- Max = Num + 1.
get_groups(Students, ProjVars, CurrProjGroups, ProjGroups, Num, Max):-
    findall(X, nth1(X,ProjVars, Num), List),
    get_groups_aux(Students, List, [], Group),
    append(CurrProjGroups, [Group], NextProjGroups),
    NextNum is Num + 1,
    get_groups(Students, ProjVars, NextProjGroups, ProjGroups, NextNum, Max).


% TESTES

constrain_group_size(Students, PreviousUCsInfo, [MinSize, MaxSize], Vars, Max):-
    
    length(Students, NumStudents),
    length(Vars, NumStudents),
    MaxNumGroups is NumStudents div MinSize,
    MinNumGroupsMod is NumStudents mod MaxSize,
    if_then_else(
                    (MinNumGroupsMod = 0),
                        (MinNumGroups is NumStudents div MaxSize),
                        (MinNumGroups is (NumStudents div MaxSize) + 1)
                ),
    
    domain([Max],MinNumGroups,MaxNumGroups),
    domain(Vars,1,MaxNumGroups),
    
    length(GroupIDs,Max),
    domain(GroupIDs, 1, Max),
    all_distinct(GroupIDs),
    
    constrain_count(Vars,[MinSize,MaxSize], Max, 1),
    constrain_worked_before(Students, PreviousUCsInfo, Vars, GroupIDs, Count),   
    %constrain_GPA(Students,Vars, Max, 1, 0, WorkedTogether),
    
    sum(Count, #=, SumCount),
    append(Vars, GroupIDs, AllVars),
    append(AllVars, [Max], AAllVars),

    labeling([minimize(SumCount),min], AAllVars),
    write(GroupIDs),nl.

constrain_count(_Vars,_,Max,Num):- Num #> Max,!.
constrain_count(Vars, [MinSize,MaxSize], Max,Num):-
    count(Num, Vars, #=, Times),
    Times #>= MinSize #/\ Times #=< MaxSize,
    NextNum is Num + 1,
    constrain_count(Vars,[MinSize,MaxSize],Max, NextNum).

if_then_else(C,I,_E):-C,!,I.
if_then_else(_C,_I,E):- E.

getGPAs(_,[],GPA,GPA).
getGPAs(Students,[H|T],GPAs,FinalGPAs):-
    nth1(H,Students,Student),   
    [N,GPA] = Student,
    append(GPAs,[GPA],NewGPAs),
    getGPAs(Students,T,NewGPAs,FinalGPAs).

constrain_GPA(_, _,Max, Num, Work,Work):- Num #> Max, !.
constrain_GPA(Students, Vars, Max, Num, WorkedTogether,FinalWorkedTogether):-
    write(Num),nl,
    findall(X, nth1(X, Vars, Num), GroupElems),
    write(GroupElems),
    getGPAs(Students,GroupElems,[],GPAs),
    min_member(MinGPA, GPAs),
    max_member(MaxGPA, GPAs),
    Diff is MaxGPA - MinGPA,
    write(Diff),nl,
    NextNum is Num + 1,
    NextWorkedTogether is Diff + WorkedTogether,
    constrain_GPA(Students,Vars, Max,NextNum, NextWorkedTogether, FinalWorkedTogether).


/**
 * haveWorkedTogether(+Student1, +Student2, +ListOfGroups)
 * 
 * True if Student1 and Student2 have worked together before according to the given information.
 * 
 * Student1 - id of student1
 * Student2 - id of student2
 * ListOfGroups - list of students that have worked together before (in other UCs or in the first project)
 */  
haveWorkedTogether(_, _, [], 1):- !.
haveWorkedTogether(Student1, Student2, [H | _T], 0):-
    element(_, H, Student1),
    element(_, H, Student2).
haveWorkedTogether(Student1, Student2, [_H | T], Res):-
    haveWorkedTogether(Student1, Student2, T, Res).

constrain_worked_before_aux(_, _, [], FinalCount, FinalCount):- !.

constrain_worked_before_aux(Students, PreviousUCsInfo, [[S1, S2] | T], CurrCount, Count):-
    element(S1, Students, Student1),
    element(S2, Students, Student2),
    haveWorkedTogether(Student1, Student2, PreviousUCsInfo, Res),
    NextCount is CurrCount + Res,
    constrain_worked_before_aux(Students, PreviousUCsInfo, T, NextCount, Count).

constrain_worked_before(_, _, _, [], []).
constrain_worked_before(Students, PreviousUCsInfo, Vars, [GroupID | RestIDs], [CurrCountH|CurrCountT]):-
    findall(X, element(X, Vars, GroupID), GroupElems),
    length(GroupElems, GroupLen),
    write(GroupLen),nl,
    if_then_else(
                    (GroupLen > 1),
                    (
                        write('Oi\n'),
                        findall(Y, comb(2,GroupElems,Y), StudentPairs),
                        constrain_worked_before_aux(Students, PreviousUCsInfo, StudentPairs, 0, GroupCount),
                        write('Fds: '), write(GroupCount), nl,
                        CurrCountH #= GroupCount
                    ),
                    CurrCountH #= 0
                ),
    constrain_worked_before(Students,PreviousUCsInfo, Vars, RestIDs, CurrCountT).

comb(0,_,[]):-!.
comb(N,[X|T],[X|Comb]):-N>0,N1 is N-1,comb(N1,T,Comb).
comb(N,[_|T],Comb):-N>0,comb(N,T,Comb).