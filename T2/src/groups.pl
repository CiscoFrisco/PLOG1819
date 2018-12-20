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





constrain_group_size(Students, GPAs, PreviousUCsInfo, [MinSize, MaxSize], Vars):-
    
    %create list of Vars with the same length of students
    length(Students, NumStudents),
    length(Vars, NumStudents),
    MaxNumGroups is NumStudents div MinSize,
    MinNumGroupsMod is NumStudents mod MaxSize,
    if_then_else(
                    (MinNumGroupsMod = 0),
                        (MinNumGroups is NumStudents div MaxSize),
                        (MinNumGroups is (NumStudents div MaxSize) + 1)
                ),
    domain([NumGroups], MinNumGroups, MaxNumGroups),
    domain(Vars, 1, MaxNumGroups),
    %write(MinNumGroups),write('-'),write(MaxNumGroups),
    %constrain group size
    maximum(NumGroups, Vars),
    constrain_count(Vars, [MinSize, MaxSize], NumGroups, 1),

    %constrain GPA
    %constrain_GPA(GPAs, Vars, NumGroups, 1, GPADiffs),
    %sum(GPADiffs, #=, SumGPADiffs),

    %constrain Worked Before
    constrain_worked_before(Students,PreviousUCsInfo,Vars,NumGroups,1),
    %sum(WorkedBefore, #=, SumWorkedBefore),

    %Min #= SumGPADiffs + SumWorkedTogether,
    %labeling
    append(Vars, [NumGroups], AllVars),
    labeling([], AllVars),
    write(SumDiffs),nl.

constrain_count(_, _, NumGroups, Num):- Num #> NumGroups.
constrain_count(Vars, [MinSize, MaxSize], NumGroups, Num):-
    count(Num, Vars, #=, Times),
    Times #>= MinSize #/\ Times #=< MaxSize,
    NextNum is Num + 1,
    constrain_count(Vars, [MinSize, MaxSize], NumGroups, NextNum).


getGPAs([], [],_,[]).
getGPAs([GPA_A | GPAs], [H|Vars], GroupID,[GroupGPAsH | GroupGPAsT]):-
    H #= GroupID #<=> B,
    GroupGPAsH #= B * GPA_A,
    getGPAs(GPAs, Vars, GroupID, GroupGPAsT).

changeZero([], _, New, New):- !.
changeZero([Head|Tail], Max, Temp, New):-
    (Head #= 0 #/\ Element #= Max)
    #\/
    (Head #\= 0 #/\ Element #= Head),
    append(Temp,[Element],Next),
    changeZero(Tail, Max, Next, New). 

constrain_GPA(_, _, NumGroups, Num, []):- Num #> NumGroups, !.
constrain_GPA(GPAs, Vars, NumGroups, Num, [DiffsH | DiffsT]):-
    getGPAs(GPAs, Vars, Num, GroupGPAs),
    maximum(MaxGPA, GroupGPAs),
    changeZero(GroupGPAs, MaxGPA, [], NewGroupGPAs),
    minimum(MinGPA, NewGroupGPAs),
    DiffsH #= MaxGPA - MinGPA,
    NextNum is Num + 1,
    constrain_GPA(GPAs, Vars, NumGroups, NextNum, DiffsT).   


haveWorkedTogether(_, _, [], 1):- !.
haveWorkedTogether(Student1, Student2, [H | _], 0):-
    element(_, H, Student1),
    element(_, H, Student2), !.
haveWorkedTogether(Student1, Student2, [_ | T], Res):-
    haveWorkedTogether(Student1, Student2, T, Res).

getStudentElems([],[],_).
getStudentElems([CurrS | RestS], Elems, GroupID):-
    CurrS #= GroupID,
    getStudentElems( RestS,  Elems, GroupID).
getStudentElems([CurrS | RestS], [CurrElem | Elems], GroupID):-
    CurrS #= GroupID,
    CurrElem #= CurrS,
    getStudentElems(RestS, Elems, GroupID).


constrain_worked_before(_, _, _, [], []):- !.
constrain_worked_before(Students, PreviousUCsInfo, Vars,  NumGroups, GroupID).
    getStudentElems(Vars, Elems, GroupID),
    comb(2,Elems,ElemPairs),
    write(Elems).



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

if_then_else(C, I, _):- C, !, I.
if_then_else(_, _, E):- E.

comb(0, _, []):- !.
comb(N, [X | T], [X | Comb]):- N>0, N1 is N-1, comb(N1, T, Comb).
comb(N, [_ | T], Comb):- N>0, comb(N, T, Comb).


