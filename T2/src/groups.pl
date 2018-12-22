:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(file_systems)).

:- consult('io.pl').

groups_ter(Students, GPAs, PreviousUCsInfo, GroupSize, Proj1Themes, Proj2Themes):-
    solve(Students, GPAs, PreviousUCsInfo, GroupSize, Proj1Vars, Proj2Vars, Max1, Max2),
    get_groups(Students, Proj1Vars, [], Proj1Groups, 1, Max1),
    get_groups(Students, Proj2Vars, [], Proj2Groups, 1, Max2),
    write('\nPROJECT 1 GROUPS\n'),
    length(Proj1Themes, Proj1ThemesLen),
    length(Proj2Themes, Proj2ThemesLen),
    write_ter(Proj1Groups, Proj1Themes, Proj1ThemesLen, 1),
    write('PROJECT 2 GROUPS\n'),
    write_ter(Proj2Groups, Proj2Themes, Proj2ThemesLen, 1). 

groups_files(CWD, StudentsFile, PreviousUCsInfoFile, Proj1ThemesFile, Proj2ThemesFile, GroupSize):-
    current_directory(_, CWD),
    read_files(StudentsFile, PreviousUCsInfoFile, Proj1ThemesFile, Proj2ThemesFile, Students, GPAs, PreviousUCsInfo, Proj1Themes, Proj2Themes),
    solve(Students, GPAs, PreviousUCsInfo, GroupSize, Proj1Vars, Proj2Vars, Max1, Max2),
    get_groups(Students, Proj1Vars, [], Proj1Groups, 1, Max1),
    get_groups(Students, Proj2Vars, [], Proj2Groups, 1, Max2),
    write_files(Proj1Groups, Proj2Groups, Proj1Themes, Proj2Themes).

get_groups_aux(_, [], Group, Group).
get_groups_aux(Students, [H | T], CurrGroup, Group):-
    nth1(H, Students, Student),
    append(CurrGroup, [Student], NextGroup),
    get_groups_aux(Students, T, NextGroup, Group).

get_groups(_, _, Groups, Groups, Num, Max):- Num > Max.
get_groups(Students, ProjVars, CurrProjGroups, ProjGroups, Num, Max):-
    findall(X, nth1(X, ProjVars, Num), List),
    get_groups_aux(Students, List, [], Group),
    append(CurrProjGroups, [Group], NextProjGroups),
    NextNum is Num + 1,
    get_groups(Students, ProjVars, NextProjGroups, ProjGroups, NextNum, Max).

% TESTES


solve(Students, GPAs, PreviousUCsInfo, [MinSize, MaxSize], Proj1Vars, Proj2Vars, NumGroups1, NumGroups2):-
    
    %create list of Vars with the same length of students
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
    domain([NumGroups1, NumGroups2], MinNumGroups, MaxNumGroups),
    domain(Proj1Vars, 1, MaxNumGroups),
    domain(Proj2Vars, 1, MaxNumGroups),

    %constrain group size
    maximum(NumGroups1, Proj1Vars),
    maximum(NumGroups2, Proj2Vars),
    constrain_count(Proj1Vars, [MinSize, MaxSize], NumGroups1, 1),
    constrain_count(Proj2Vars, [MinSize, MaxSize], NumGroups2, 1),

    %constrain GPA
    constrain_GPA(GPAs, Proj1Vars, NumGroups1, 1, GPADiffs1),
    constrain_GPA(GPAs, Proj2Vars, NumGroups2, 1, GPADiffs2),
    sum(GPADiffs1, #=, SumGPADiffs1),
    sum(GPADiffs2, #=, SumGPADiffs2),

    %constrain Worked Before
    constrain_worked_before(Students, PreviousUCsInfo, Proj1Vars, WorkedBefore1),
    sum(WorkedBefore1, #=, SumWorkedBefore1),
    constrain_worked_before(Students, PreviousUCsInfo, Proj2Vars, WorkedBefore2),
    sum(WorkedBefore2, #=, SumWorkedBefore2),
    constrain_worked_first_project(Students, Proj1Vars, Proj2Vars, 1, NumGroups1),
    Min #= SumGPADiffs1 + SumGPADiffs2 + SumWorkedBefore1 + SumWorkedBefore2,
    
    %labeling
    append(Proj1Vars, Proj2Vars, AllVars),
    labeling([minimize(Min)], AllVars).


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
    constrain_GPA(GPAs, Vars, NumGroups, 1, GPADiffs),
    sum(GPADiffs, #=, SumGPADiffs),

    %constrain Worked Before
    constrain_worked_before(Students, PreviousUCsInfo, Vars, WorkedBefore),
    sum(WorkedBefore, #=, SumWorkedBefore),
    write(WorkedBefore),nl,
    write(SumWorkedBefore),nl,
    Min #= SumGPADiffs + SumWorkedBefore,
    %labeling
    append(Vars, [NumGroups], AllVars),
    labeling([minimize(Min)], AllVars),
    write(SumWorkedBefore),nl.

constrain_count(_, _, NumGroups, Num):-Num #> NumGroups.
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

getGroupIDs([],[],_,[]).
getGroupIDs([CS | RS], [_ | RV], [S1,S2],RestID):-
    CS \= S1 , 
    CS \= S2,
    getGroupIDs( RS,  RV, [S1, S2], RestID).
getGroupIDs([CS | RS], [CV | RV], [S1,S2],[CurrID|RestID]):-
    (CS = S1 ; CS = S2) ,
    CurrID #= CV,
    getGroupIDs( RS,  RV, [S1, S2], RestID).

constrain_worked_before(_, [], _, []).
constrain_worked_before(Students, [CurrPair | RestPairs], Vars, [PairWT | RestWT]):-
    getGroupIDs(Students, Vars,CurrPair, CurrIDs),
    nvalue(DistinctMembers,CurrIDs),    
    DistinctMembers #= 1 #<=>B,
    PairWT #= B,
    constrain_worked_before(Students,  RestPairs, Vars,  RestWT).

get_group([], _, _, []).
get_group([H1 | T1], [_ | T2] ,Num,  Rest):-
    H1 #\= Num,
    get_group(T1, T2, Num, Rest).
get_group([H1 | T1], [H2 | T2], Num, [Elem | Rest]):-
    H1 #= Num,
    Elem #= H2,
    get_group(T1, T2, Num, Rest).

constrain_worked_first_project(_, _, _, Num, NumGroups):- Num #> NumGroups.
constrain_worked_first_project(Students, Proj1Vars, Proj2Vars, Num, NumGroups):-
    get_group(Proj1Vars, Proj2Vars, Num, SameGroup2),
    all_distinct(SameGroup2),
    NextNum is Num + 1,
    constrain_worked_first_project(Students, Proj1Vars, Proj2Vars, NextNum, NumGroups).

if_then_else(C, I, _):- C, !, I.
if_then_else(_, _, E):- E.