%['/Users/max/Dropbox/Unif/Master/DeclarativeProgramming/SecondYear/Project/DeclarativeProgramming/project.pl'].

:- use_module(library(clpfd)).

% --
% -- MAIN PROGRAM
% --

% Teacher
prof --> ["prof"].
teacherName(Name) --> [Name], {string(Name)}.
teacher(Name) --> prof, teacherName(Name).
teacher(Name) --> ["he", "also"], {Name = "he"}.
teacher(Name) --> ["she", "also"], {Name = "she"}.

% Class
classWord --> ["class"].
classWord --> ["classes"].
classNumber(Number) --> [Number], {string(Number)}.
class(ClassList) --> classWord, classNumber(ClassOne), classNumber(ClassTwo), conjonction, classNumber(ClassThree).
class([ClassOne, ClassTwo]) --> classWord, classNumber(ClassOne), conjonction, classNumber(ClassTwo).
class([Number]) --> classWord, classNumber(Number).
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


% Sentence description
teacherSentence(StackIn, StackOut) --> teacher(Name), teacherDescription, class(ClassList), { addToStack(StackIn, Name, ClassList, StackOut) }.
classSentence(StackIn, StackOut) --> class(ClassList), classDescription, room.
classSentence(StackIn, StackOut) --> class(ClassList), classDescription.
classSentence(StackIn, StackOut) --> class(ClassList), classDescription, class(ClassList2).
roomSentence --> room, roomDescription, students.

% Full Sentence
%testSentence(StackIn, StackOut) --> 

%% emptyStack(X)
%
% emptyStack/1 Creates a new empty stack
%
% @param X The new stack
emptyStack(X) :- X = [].

%% addToStack()
addToStack(Stack, Subject, Var, StackOut) :-
	StackOut = [(Subject, Var)|Stack].



% --
% -- TEST
% --
:- begin_tests(base).

test(prof) :-
	phrase(teacher("smith"), ["prof", "smith"]),
	phrase(teacher(_), ["he", "also"]).

test(teaching) :-
	phrase(teacherSentence(_,_), ["prof", "rob", "teaches", "class", "c1"]),
	phrase(teacherSentence(_,_), ["he", "also", "teaches", "class", "c2"]).

test(class) :-
	phrase(classSentence(_,_), ["class", "c1", "is", "in", "room", 102]),
	phrase(classSentence(_,_), ["class", "c1", "is", "before", "class", "c2"]),
	phrase(classSentence(_,_), ["classes", "c1", "and", "c2", "are", "in", "the", "same", "room"]),
	phrase(classSentence(_,_), ["classes", "c1", "c2", "and", "c3", "have", "the", "same", "teacher"]).

test(room) :-
	phrase(roomSentence, ["room", 102, "seats", 100, "students"]).

test(stackTeacher) :-
	emptyStack(Stack),
	phrase(teacherSentence(Stack,[("rob",["c1"])]), ["prof", "rob", "teaches", "class", "c1"]),
	phrase(teacherSentence(Stack,[("he",["c1", "c2"])]), ["he", "also", "teaches", "classes", "c1", "and", "c2"]).


