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
    solve(Students, PreviousUCsInfo, GroupSize, Proj1Vars, Proj2Vars, Max),
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
    findall(X, nth1(X, ProjVars, Num), List),
    get_groups_aux(Students, List, [], Group),
    append(CurrProjGroups, [Group], NextProjGroups),
    NextNum is Num + 1,
    get_groups(Students, ProjVars, NextProjGroups, ProjGroups, NextNum, Max).

% TESTES

constrain_group_size(Students, GPAs, PreviousUCsInfo, [MinSize, MaxSize], Vars, Max):-
    
    length(Students, NumStudents),
    length(Vars, NumStudents),
    MaxNumGroups is NumStudents div MinSize,
    MinNumGroupsMod is NumStudents mod MaxSize,
    if_then_else(
                    (MinNumGroupsMod = 0),
                        (MinNumGroups is NumStudents div MaxSize),
                        (MinNumGroups is (NumStudents div MaxSize) + 1)
                ),
    domain([Max], MinNumGroups, MaxNumGroups),
    domain(Vars, 1, MaxNumGroups),
    
    % length(GroupIDs, Max),
    % domain(GroupIDs, 1, Max),
    % all_distinct(GroupIDs),
    nvalue(Max, Vars),
    constrain_count(Vars, [MinSize, MaxSize], Max, 1),
    %constrain_worked_before(Students, PreviousUCsInfo, Vars, 1, Max, Count),   
    % constrain_GPA(GPAs, Vars, Max, 1, Diffs),
    % sum(Diffs, #=, SumDiffs),
    %sum(Count, #=, SumCount),
    %Min #= SumDiffs + SumCount,
    append(Vars, [Max], AllVars),
    % append(AllVars, [Max], AAllVars),

    labeling([], AllVars).

solve(Students, PreviousUCsInfo, [MinSize, MaxSize], Proj1Vars, Proj2Vars, Max):-
    length(Students, NumStudents),
    length(Proj1Vars, NumStudents),
    length(Proj2Vars, NumStudents),
    MaxNumGroups is NumStudents div MinSize,
    MinNumGroupsMod is NumStudents mod MaxSize,
    if_then_else(
                    (MinNumGroupsMod = 0),
                        (MinNumGroups is NumStudents div MaxSize),
                        (MinNumGroups is (NumStudents div MaxSize) + 1)
                ),
    domain([Max], MinNumGroups, MaxNumGroups),
    domain(Proj1Vars, 1, MaxNumGroups),
    domain(Proj2Vars, 1, MaxNumGroups),
    length(GroupIDs1, Max),
    domain(GroupIDs1, 1, Max),
    length(GroupIDs2, Max),
    domain(GroupIDs2, 1, Max),
    all_distinct(GroupIDs1),
    constrain_count(Proj1Vars,[MinSize, MaxSize], Max, 1),
    constrain_count(Proj2Vars,[MinSize, MaxSize], Max, 1),
    constrain_worked_before(Students, PreviousUCsInfo, Proj1Vars, GroupIDs1, Count),
    constrain_worked_before(Students, PreviousUCsInfo, Proj2Vars, GroupIDs2, Count2),   
    constrain_GPA(Students, Proj1Vars, Max, 1, Diffs1),
    constrain_GPA(Students, Proj2Vars, Max, 1, Diffs2),
    constrain_worked_first_project(Students, Proj1Vars, Proj2Vars, GroupIDs2),
    sum(Diffs1, #=, SumDiffs),
    sum(Count, #=, SumCount),
    sum(Diffs2, #=, SumDiffs2),
    sum(Count2, #=, SumCount2),
    Min #= SumDiffs + SumCount + SumDiffs2 + SumCount2,
    append(Proj1Vars, Proj2Vars, ProjVars),
    append(ProjVars, [Max], AllVars),
    labeling([minimize(Min), min], AllVars).

constrain_count(_, _, Max, Num):- Num #> Max.
constrain_count(Vars, [MinSize, MaxSize], Max, Num):-
    count(Num, Vars, #=, Times),
    Times #>= MinSize #/\ Times #=< MaxSize,
    NextNum is Num + 1,
    constrain_count(Vars, [MinSize, MaxSize], Max, NextNum).

if_then_else(C, I, _):- C, !, I.
if_then_else(_, _, E):- E.

getGPAs(_, [], []).
getGPAs(GPAs, [H|T], [GroupGPAsH | GroupGPAsT]):-
    element(H, GPAs, GPA),   
    GroupGPAsH #= GPA,
    getGPAs(GPAs, T, GroupGPAsT).

constrain_GPA(_, _, Max, Num, []):- Num #> Max, !.
constrain_GPA(GPAs, Vars, Max, Num, [DiffsH | DiffsT]):-
    findall(X, element(X, Vars, Num), GroupElems),
    getGPAs(GPAs, GroupElems, [], GroupGPAs),
    minimum(MinGPA, GroupGPAs),
    maximum(MaxGPA, GroupGPAs),
    NextNum is Num + 1,
    DiffsH #= MaxGPA - MinGPA,
    constrain_GPA(GPAs, Vars, Max, NextNum, DiffsT).


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
haveWorkedTogether(Student1, Student2, [H | _], 0):-
    element(_, H, Student1),
    element(_, H, Student2), !.
haveWorkedTogether(Student1, Student2, [_ | T], Res):-
    haveWorkedTogether(Student1, Student2, T, Res).

constrain_worked_before_aux(_, _, [], FinalCount, FinalCount):- !.

constrain_worked_before_aux(Students, PreviousUCsInfo, [[S1, S2] | T], CurrCount, Count):-
    element(S1, Students, Student1),
    element(S2, Students, Student2),
    haveWorkedTogether(Student1, Student2, PreviousUCsInfo, Res),
    NextCount is CurrCount + Res,
    constrain_worked_before_aux(Students, PreviousUCsInfo, T, NextCount, Count).

constrain_worked_before(_, _, _, [], []):- !.
constrain_worked_before(Students, PreviousUCsInfo, Vars, [GroupID | RestIDs], [CurrCountH | CurrCountT]):-
    count(GroupID, Vars, #=, GroupLen),
    length(GroupElems, GroupLen),
    findall(X, element(X, Vars, GroupID), GroupElems),
    format('Num: ~d~n', [GroupID]),nl,
    format('GroupLen: ~d~n', [GroupLen]),nl,
    if_then_else(
                    (GroupLen > 1),
                    (
                        write('Oi\n'),
                        findall(Y, comb(2, GroupElems, Y), StudentPairs),
                        constrain_worked_before_aux(Students, PreviousUCsInfo, StudentPairs, 0, GroupCount),
                        write('Fds: '), write(GroupCount), nl,
                        CurrCountH #= GroupCount
                    ),
                    CurrCountH #= 0
                ),
    constrain_worked_before(Students, PreviousUCsInfo, Vars, RestIDs, CurrCountT).

comb(0, _, []):- !.
comb(N, [X | T], [X | Comb]):- N>0, N1 is N-1, comb(N1, T, Comb).
comb(N, [_ | T], Comb):- N>0, comb(N, T, Comb).

constrain_worked_first_project_aux([], _, []).
constrain_worked_first_project_aux([H | T], Proj2Vars, [SameGroupH | SameGroupT]):-
    element(H, Proj2Vars, X),
    SameGroupH #= X,
    constrain_worked_first_project_aux(T, Proj2Vars, SameGroupT).

constrain_worked_first_project(_, _, _, []).
constrain_worked_first_project(Students, Proj1Vars, Proj2Vars, [GroupID | RestIDs]):-
    findall(X, element(X, Proj1Vars, GroupID), GroupElems),
    constrain_worked_first_project_aux(GroupElems, Proj2Vars, SameGroup2),
    all_distinct(SameGroup2),
    constrain_worked_first_project(Students, Proj1Vars, Proj2Vars, RestIDs).