%['/Users/max/Dropbox/Unif/Master/DeclarativeProgramming/SecondYear/Project/project.pl'].

:- use_module(library(clpfd)).

% --
% -- MAIN PROGRAM
% --

% Teacher
prof --> ["prof"].
teacherName(Name) --> [Name], {string(Name)}.
teacher --> prof, teacherName(Name).
teacher --> ["he", "also"].
teacher --> ["she", "also"].

% Class
class --> ["class"].
class --> ["classes"].
classNumber(Number) --> [Number], {string(Number)}.
class --> class, classNumber(Number).
%TODO: add several class

% Room
room --> ["room"].
roomNumber(Number) --> [Number], {string(Number)}. %TODO: change string
room --> room, roomNumber(Number).

teacherDescription --> ["teaches", "class"].
teacherDescription --> ["teaches", "classes"].
classDescription --> ["is", "in", "room"].
classDescription --> ["are", "in", "the", "same", "room"].
classDescription --> ["have", "the", "same", "teacher"].
classDescription --> ["has"].
classDescription --> ["is", "before"].
classDescription --> ["is", "after"].
classDescription --> ["are", "on", "the", "same", "day"].
roomDescription --> ["seats"].
descriptionConnexion --> ["and"].


% --
% -- TEST
% --
:- begin_tests(base).

test(prof) :-
	phrase(teacher, ["prof", "smith"]),
	phrase(teacher, ["he", "also"]).
test(room) :-
	phrase(room, ["room", "smith"]).


