%['/Users/max/Dropbox/Unif/Master/DeclarativeProgramming/SecondYear/Project/DeclarativeProgramming/project.pl'].

:- use_module(library(clpfd)).

%% ------ TODO LIST -----
% X) 	: generate goal --> teacher(jones, [a1,b2,c3])
% 1.1) 	: find way to find fitting goal -> teacher(...) class(...)
% 2) 	: find way to check "he also" after prof X 
% -----


%% ------ QUESTIONS LIST ------
% 1) Split class sentence in order to push to stack the right goal ?
% -> 

%% ------ DRAFT IDEAS -----
% teach : teacher(prof, [courses])
% class : classIn([classes], room)
% class : teacherBy([classes], prof)
% class : populate(class, nbr_students)
% class : before(class1, class2)
% class : after(class1, class2)
% class : sameDay([classes])
% room 	: seats(room, nbr_students)

% --
% -- MAIN PROGRAM
% --

% Teacher
prof --> [prof].
%teacherName(Name) --> [Name], {string(Name)}.
teacherName(Name) --> [Name].
teacher(Name) --> prof, teacherName(Name).
teacher(Name) --> [he, also], {Name = he}.
teacher(Name) --> [she, also], {Name = she}.

% Class
classWord --> [class].
classWord --> [classes].
%classNumber(Number) --> [Number], {string(Number)}.
classNumber(Number) --> [Number].
class([ClassOne, ClassTwo, ClassThree]) --> classWord, classNumber(ClassOne), classNumber(ClassTwo), conjonction, classNumber(ClassThree).
class([ClassOne, ClassTwo]) --> classWord, classNumber(ClassOne), conjonction, classNumber(ClassTwo).
class([Number]) --> classWord, classNumber(Number).
%TODO: add several classes in a better way

% Room
roomWord --> [room].
roomNumber(Number) --> [Number], {integer(Number)}.
room(RoomNumber) --> roomWord, roomNumber(RoomNumber).

% Students
studentsWord --> [students].
numberStudents(Number) --> [Number], {integer(Number)}.
students(Number) --> numberStudents(Number), studentsWord.

% Subjects actions
teacherDescription --> [teaches].
classDescription --> [is, in].
classDescription --> [are, in, the, same, room].
classDescription --> [have, the, same, teacher].
classDescription --> [has].
classDescription --> [is, before].
classDescription --> [is, after].
classDescription --> [are, on, the, same, day].
roomDescription --> [seats].
conjonction --> [and].


% Sentence description
teacherSentence(StackIn, StackOut) --> teacher(Name), teacherDescription, class(ClassList), { addTeacherToStack(StackIn, Name, ClassList, StackOut) }.
classSentence(StackIn, StackOut) --> class(ClassList), classDescription, room(RoomNumber).
classSentence(StackIn, StackOut) --> class(ClassList), classDescription.
classSentence(StackIn, StackOut) --> class(ClassList), classDescription, class(ClassList2).
roomSentence(StackIn, StackOut) --> room(RoomNumber), roomDescription, students(StudentsNumber), { addRoomToStack(StackIn, RoomNumber, StudentsNumber, StackOut) }.

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

%% addTeacherToStack(Stack, Subject, Var Stackout)
%
% addTeacherToStack/4 Add teacher goal to the stack
%
% @param Stack Current Stack
% @parem Subject Subject of the parsed sentence
% @param Var COD of the sentence
% @param StackOut Updated Stack
%
addTeacherToStack(Stack, Subject, Var, StackOut) :-
	StackOut = [(teacher(Subject, Var))|Stack].

%% addRoomToStack
%
% addRoomToStack Add room goal seating student to the Stack
%
addRoomToStack(Stack, RoomName, Seats, [(seats(RoomName, Seats))|Stack]).


% --
% -- TEST
% --
:- begin_tests(base).

test(prof) :-
	phrase(teacher(smith), [prof, smith]),
	phrase(teacher(_), [he, also]).

test(teaching) :-
	phrase(teacherSentence(_,_), [prof, rob, teaches, class, c1]),
	phrase(teacherSentence(_,_), [he, also, teaches, class, c2]).

test(class) :-
	phrase(classSentence(_,_), [class, c1, is, in, room, 102]),
	phrase(classSentence(_,_), [class, c1, is, before, class, c2]),
	phrase(classSentence(_,_), [classes, c1, and, c2, are, in, the, same, room]),
	phrase(classSentence(_,_), [classes, c1, c2, and, c3, have, the, same, teacher]).

test(room) :-
	phrase(roomSentence(_,_), [room, 102, seats, 100, students]).

test(stackTeacher) :- 
	emptyStack(Stack),
	phrase(teacherSentence(Stack,[teacher(rob, [c1])]), [prof, rob, teaches, class, c1]),	
	phrase(teacherSentence(Stack,[teacher(steve, [c1,c2])]), [prof, steve, teaches, classes, c1, and, c2]).	

test(stackRoom) :-
	emptyStack(Stack),
	phrase(roomSentence(Stack, [seats(102, 100)]), [room, 102, seats, 100, students]).







