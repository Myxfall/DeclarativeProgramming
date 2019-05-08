%['/Users/max/Dropbox/Unif/Master/DeclarativeProgramming/SecondYear/Project/DeclarativeProgramming/project.pl'].

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
classWord --> ["class"].
classWord --> ["classes"].
classNumber(Number) --> [Number], {string(Number)}.
class --> classWord, classNumber(Number), classNumber(NumberTwo), conjonction, classNumber(NumberThird).
class --> classWord, classNumber(Number), conjonction, classNumber(NumberBis).
class --> classWord, classNumber(Number).
%TODO: add several classes in a better way

% Room
roomWord --> ["room"].
roomNumber(Number) --> [Number], {integer(Number)}.
room --> roomWord, roomNumber(Number).

% Students
studentsWord --> ["students"].
numberStudents(Number) --> [Number], {integer(Number)}.
students --> numberStudents(Number), studentsWord.

% Subjects actions
teacherDescription --> ["teaches"].
classDescription --> ["is", "in"].
classDescription --> ["are", "in", "the", "same", "room"].
classDescription --> ["have", "the", "same", "teacher"].
classDescription --> ["has"].
classDescription --> ["is", "before"].
classDescription --> ["is", "after"].
classDescription --> ["are", "on", "the", "same", "day"].
roomDescription --> ["seats"].
conjonction --> ["and"].
conjonction --> [].

% Sentence description
teacherSentence --> teacher, teacherDescription, class.
classSentence --> class, classDescription, room.
classSentence --> class, classDescription.
classSentence --> class, classDescription, class.
roomSentence --> room, roomDescription, students.


% --
% -- TEST
% --
:- begin_tests(base).

test(prof) :-
	phrase(teacher, ["prof", "smith"]),
	phrase(teacher, ["he", "also"]).

test(teaching) :-
	phrase(teacherSentence, ["prof", "rob", "teaches", "class", "c1"]),
	phrase(teacherSentence, ["he", "also", "teaches", "class", "c2"]).

test(class) :-
	phrase(classSentence, ["class", "c1", "is", "in", "room", 102]),
	phrase(classSentence, ["class", "c1", "is", "before", "class", "c2"]),
	phrase(classSentence, ["classes", "c1", "and", "c2", "are", "in", "the", "same", "room"]),
	phrase(classSentence, ["classes", "c1", "c2", "and", "c3", "have", "the", "same", "teacher"]).

test(room) :-
	phrase(roomSentence, ["room", 102, "seats", 100, "students"]).

