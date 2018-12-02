:- use_module(library(clpfd)).
:- use_module(library(lists)).

/**
 * groups(+Students, +Proj1Themes, +Proj2Themes, +GroupSize, +PreviousUCsInfo, -Proj1Groups, -Proj2Groups)
 * 
 * Students - list of students (name/id and their gpa)
 * Proj1Themes - list of available themes for the first project
 * GroupSize - interval representing the possible group sizes
 * Proj2Themes - list of available themes for the second project
 * PreviousUCsInfo - list of students that have worked together before
 * Proj1Groups - list of groups for the first project
 * Proj2Groups - list of groups for the second project
 */ 
groups(Students, Proj1Themes, Proj2Themes, GroupSize, PreviousUCsInfo, Proj1Groups, Proj2Groups).

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