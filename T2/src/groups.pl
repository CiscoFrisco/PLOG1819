:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(file_systems)).

:- consult('io.pl').


reset_timer :- statistics(walltime,_).	
print_time :-
	statistics(walltime,[_,T]),
	TS is ((T//10)*10)/1000,
	nl, write('Time: '), write(TS), write('s'), nl, nl.


if_then_else(C, I, _):- C, !, I.
if_then_else(_, _, E):- E.


groups_ter(Students, GPAs, PreviousUCsInfo, GroupSize, Proj1Themes, Proj2Themes):-
    solve(Students, GPAs, PreviousUCsInfo, GroupSize, Proj1Vars, Proj2Vars),
    max_member(NumGroups1, Proj1Vars),
    max_member(NumGroups2, Proj2Vars),
    get_groups(Students, Proj1Vars, [], Proj1Groups, 1, NumGroups1),
    get_groups(Students, Proj2Vars, [], Proj2Groups, 1, NumGroups2),
    write('\nPROJECT 1 GROUPS\n'),
    length(Proj1Themes, Proj1ThemesLen),
    length(Proj2Themes, Proj2ThemesLen),
    write_ter(Proj1Groups, Proj1Themes, Proj1ThemesLen, 0),
    write('PROJECT 2 GROUPS\n'),
    write_ter(Proj2Groups, Proj2Themes, Proj2ThemesLen, 0), !. 

groups_files(CWD, StudentsFile, PreviousUCsInfoFile, Proj1ThemesFile, Proj2ThemesFile, GroupSize):-
    current_directory(_, CWD),
    read_files(StudentsFile, PreviousUCsInfoFile, Proj1ThemesFile, Proj2ThemesFile, Students, GPAs, PreviousUCsInfo, Proj1Themes, Proj2Themes),
    solve(Students, GPAs, PreviousUCsInfo, GroupSize, Proj1Vars, Proj2Vars),
    max_member(NumGroups1, Proj1Vars),
    max_member(NumGroups2, Proj2Vars),
    get_groups(Students, Proj1Vars, [], Proj1Groups, 1, NumGroups1),
    get_groups(Students, Proj2Vars, [], Proj2Groups, 1, NumGroups2),
    write_files(Proj1Groups, Proj2Groups, Proj1Themes, Proj2Themes), !.

get_groups_aux(_, [], Group, Group):-!.
get_groups_aux(Students, [H | T], CurrGroup, Group):-
    nth1(H, Students, Student),
    append(CurrGroup, [Student], NextGroup),
    get_groups_aux(Students, T, NextGroup, Group).

get_groups(_, _, Groups, Groups, Num, Max):- Num > Max, !.
get_groups(Students, ProjVars, CurrProjGroups, ProjGroups, Num, Max):-
    findall(X, nth1(X, ProjVars, Num), List),
    get_groups_aux(Students, List, [], Group),
    append(CurrProjGroups, [Group], NextProjGroups),
    NextNum is Num + 1,
    get_groups(Students, ProjVars, NextProjGroups, ProjGroups, NextNum, Max).

solve(Students, GPAs, PreviousUCsInfo, [MinSize, MaxSize], Proj1Vars, Proj2Vars):-
    
    %create list of Vars with the same length of students
    length(Students, NumStudents),
    length(Proj1Vars, NumStudents),
    length(Proj2Vars, NumStudents),

    %calculate maximum and minimum number of groups
    MaxNumGroups is NumStudents div MinSize,
    MinNumGroupsMod is NumStudents mod MaxSize,
    if_then_else(
                    (MinNumGroupsMod = 0),
                        (MinNumGroups is NumStudents div MaxSize),
                        (MinNumGroups is (NumStudents div MaxSize) + 1)
                ),

    %domain variables' domain
    domain(Proj1Vars, 1, MaxNumGroups),
    domain(Proj2Vars, 1, MaxNumGroups),

    %constrain group size
    get_elems(Proj1Vars, [MinSize, MaxSize], MaxNumGroups, 1,Proj1GroupElems),
    get_elems(Proj2Vars, [MinSize, MaxSize], MaxNumGroups, 1,Proj2GroupElems),
    constrain_size(Proj1GroupElems,[MinSize,MaxSize]),
    constrain_size(Proj2GroupElems,[MinSize,MaxSize]),
    count(0,Proj1GroupElems,#=, Zeros1),
    count(0,Proj2GroupElems,#=, Zeros2),
    maximum(Max1, Proj1Vars),
    maximum(Max2, Proj2Vars),
    Max1 #= MaxNumGroups - Zeros1,
    Max2 #= MaxNumGroups - Zeros2,

    %constrain GPA
    constrain_GPA(GPAs, Proj1Vars, MaxNumGroups, 1, GPADiffs1),
    constrain_GPA(GPAs, Proj2Vars, MaxNumGroups, 1, GPADiffs2),
    sum(GPADiffs1, #=, SumGPADiffs1),
    sum(GPADiffs2, #=, SumGPADiffs2),

    %constrain Worked Before
    constrain_worked_before(Students, PreviousUCsInfo, Proj1Vars, WorkedBefore1),
    sum(WorkedBefore1, #=, SumWorkedBefore1),
    constrain_worked_before(Students, PreviousUCsInfo, Proj2Vars, WorkedBefore2),
    sum(WorkedBefore2, #=, SumWorkedBefore2),

    %different from first project
    constrain_worked_first_project(Proj1Vars, Proj2Vars, MaxNumGroups, 1),
    
    %minimize variable
    Min #= SumGPADiffs1 + SumWorkedBefore1 + SumGPADiffs2 + SumWorkedBefore2,

    %labeling

    append(Proj1Vars, Proj2Vars, AllVars),
    reset_timer,
    labeling([minimize(Min)], AllVars),

    print_time,
    fd_statistics.


solve_only_first(Students, GPAs, PreviousUCsInfo, [MinSize, MaxSize], Vars):-
    
    %create list of Vars with the same length of students
    length(Students, NumStudents),
    length(Vars, NumStudents),
    
    %calculate the maximum and minimum number of groups of the given input
    MaxNumGroups is NumStudents div MinSize,
    MinNumGroupsMod is NumStudents mod MaxSize,
    if_then_else(
                    (MinNumGroupsMod = 0),
                        (MinNumGroups is NumStudents div MaxSize),
                        (MinNumGroups is (NumStudents div MaxSize) + 1)
                ),
    
    %domain variables' domain
    domain(Vars, 1, MaxNumGroups),
    
    %constrain group size
    get_elems(Vars, [MinSize, MaxSize], MaxNumGroups, 1, GroupElems),
    constrain_size(GroupElems, [MinSize,MaxSize]),
    count(0,GroupElems,#=, Zeros),
    maximum(Max, Vars),
    Max #= MaxNumGroups - Zeros,
    
    %constrain GPA
    constrain_GPA(GPAs, Vars, MaxNumGroups, 1, GPADiffs),
    sum(GPADiffs, #=, SumGPADiffs),

    %constrain Worked Before
    constrain_worked_before(Students, PreviousUCsInfo, Vars, WorkedBefore),
    sum(WorkedBefore, #=, SumWorkedBefore),

    %value to minimize
    Min #= SumGPADiffs + SumWorkedBefore,
    
    %labeling   
    reset_timer,
    labeling([minimize(Min),up], Vars),
    print_time,
    fd_statistics.


constrain_size([],_).
constrain_size([H | T],[MinSize,MaxSize]):-
    (H #>= MinSize #/\ H #=< MaxSize) #\/ (H #= 0),
    constrain_size( T, [MinSize,MaxSize]).


get_elems(_, _, NumGroups,Num,[]):- Num > NumGroups, !.
get_elems(Vars, [MinSize, MaxSize], NumGroups, Num,[Times | T]):-
    count(Num, Vars, #=, Times),
    NextNum is Num + 1,
    get_elems(Vars, [MinSize, MaxSize], NumGroups, NextNum, T). 


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


constrain_GPA(_, _, NumGroups, Num, []):- Num > NumGroups, !.
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
    CS \= S1, CS \= S2, !,
    getGroupIDs( RS,  RV, [S1, S2], RestID).
getGroupIDs([CS | RS], [CV | RV], [S1,S2],[CurrID|RestID]):-
    (CS = S1 ; CS = S2) , !,
    CurrID #= CV,
    getGroupIDs( RS,  RV, [S1, S2], RestID).


constrain_worked_before(_, [], _, []).
constrain_worked_before(Students, [CurrPair | RestPairs], Vars, [PairWT | RestWT]):-
    getGroupIDs(Students, Vars,CurrPair, CurrIDs),
    nvalue(DistinctMembers,CurrIDs),    
    DistinctMembers #= 1 #<=> PairWT,
    constrain_worked_before(Students,  RestPairs, Vars,  RestWT).


get_group([], _, _, [],1).
get_group([H1 | T1], [H2 | T2], Num, [Elem | Rest],GL):-
    H1 #= Num #<=> B,
    Elem #= B * H2,
    GL #= B + OldGL,
    get_group(T1, T2, Num, Rest,OldGL).


constrain_worked_first_project(_, _, NumGroups, Num):- Num > NumGroups,!.
constrain_worked_first_project(Proj1Vars, Proj2Vars, NumGroups, Num):-
    get_group(Proj1Vars, Proj2Vars, Num, SameGroup,GroupLen),
    nvalue(DistinctMembers,SameGroup),
    DistinctMembers #= GroupLen,
    NextNum is Num + 1,
    constrain_worked_first_project(Proj1Vars, Proj2Vars, NumGroups, NextNum).