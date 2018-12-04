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
groups(Students, NumThemes1, NumThemes2, GroupSize, PreviousUCsInfo, Proj1Groups, Proj2Groups).

/**
 * main(+CWD, +StudentsFile, +NumThemes1, +NumThemes2, +PreviousUCsInfoFile, +GroupSize)
 * 
 * CWD - current working directory
 * StudentsFile - students file path relative to cwd
 * NumThemes1 - number of available themes for the first project
 * NumThemes2 - number of available themes for the second project
 * PreviousUCsInfoFile - previousUCsInfo file path relative to cwd
 * GroupSize - interval representing the possible group sizes
 */ 
main(CWD, StudentsFile, NumThemes1, NumThemes2, PreviousUCsInfoFile, GroupSize):-
    current_directory(_, CWD),
    read_files(StudentsFile, PreviousUCsInfoFile, Students, PreviousUCsInfo),
    groups(Students, NumThemes1, NumThemes2, GroupSize, PreviousUCsInfo, Proj1Groups, Proj2Groups),
    write_files(Proj1Groups, Proj2Groups).

/**
 * haveWorkedTogether(+Student1, +Student2, +PreviousUCsInfo)
 * 
 * True if Student1 and Student2 have worked together before according to the given information.
 * 
 * Student1 - id of student1
 * Student2 - id of student2
 * PreviousUCsInfo - list of students that have worked together before
 */  
haveWorkedTogether(Student1, Student2, PreviousUCsInfo):-
    member([Student1, Student2], PreviousUCsInfo);
    member([Student2, Student1], PreviousUCsInfo).