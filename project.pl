%['/Users/max/Dropbox/Unif/Master/DeclarativeProgramming/SecondYear/Project/DeclarativeProgramming/project.pl'].

:- use_module(library(clpfd)).

%% ------ TODO LIST -----
% X) 	: generate goal --> teacher(jones, [a1,b2,c3])
% X) 	: find way to find fitting goal -> teacher(...) class(...)
% X) 	: find way to check "he also" after prof X 
% X) 	: to do that, updateTeacher(...) that find teacher and adds the courses after finding "he also"
% X)	: just pop() stack and get the goal teacher(...), if not teacher throw error
% 3) 	: pour creer containtes : listes de class/room/teacher and put inside the elements
% 4) 	: he/she should accept more than just one new class
% 5)	: parser that works with sentences plain text
% 6)	: goals should have input/output for system constaintes, find what kind
% 7) 	: create a new stack that contains the variables name ?
% 8) 	: 
% -----


%% ------ QUESTIONS LIST ------
% 1) Split class sentence in order to push to stack the right goal ?
% -> yes
% 2) there is no information about class duration, how are we suppose to give 2 hours of prepration time to teacher then ?
% ->
% 3) Teacher should teach each class every week, I suppose we re talking about HIS classes and not all.

%% ------ DRAFT IDEAS -----

% maybe possible de créer directe des contraintes comme before(c1, c2) -> c1 #< c2
% ou le faire après au cas par cas
% return of everything is a triple list with teacher class room that it is possible to print

% -- Contraintes --
% list of teacher associated with number and class - 1 list of constaintes
% list of room associated with seats - 2 list of constraintes
% list of classes
% then go throught the list and creates constaintes, as the box example

% ?les contraintes doivent être construites au fur et à mesure. A la fin on récupère une liste quil suffit de print

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
teacherSuppl --> [he, also].
teacherSuppl --> [she, also].

% Class
classWord --> [class].
classWord --> [classes].
%classNumber(Number) --> [Number], {string(Number)}.
classNumber(Number) --> [Number].
class([ClassOne, ClassTwo, ClassThree]) --> classWord, classNumber(ClassOne), classNumber(ClassTwo), conjonction, classNumber(ClassThree).
class([ClassOne, ClassTwo]) --> classWord, classNumber(ClassOne), conjonction, classNumber(ClassTwo).
class([Number]) --> classWord, classNumber(Number).
% TODO: add several classes in a better way

% Room
roomWord --> [room].
roomNumber(Number) --> [Number], {integer(Number)}.
room(RoomNumber) --> roomWord, roomNumber(RoomNumber).

% Students
studentsWord --> [students].
numberStudents(Number) --> [Number], {integer(Number)}.
students(Number) --> numberStudents(Number), studentsWord.

% Subjects actions
% TODO: remove this section
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
%teacherSentence(StackIn, StackOut) --> teacher(Name), teacherDescription, class(ClassList), { addTeacherToStack(StackIn, Name, ClassList, StackOut) }.
teacherSentence(StackIn, StackOut) --> teacher(Name), teacherDescription, class(ClassList), { addToStack(StackIn, teacher(Name, ClassList), StackOut) }.
teacherSentence(StackIn, StackOut) --> teacherSuppl, teacherDescription, class([Class]), { updateStack(StackIn, Class, StackOut) }.

%% Class sentence really general
%classSentence(StackIn, StackOut) --> class(ClassList), classDescription, room(RoomNumber).
%classSentence(StackIn, StackOut) --> class(ClassList), classDescription.
%classSentence(StackIn, StackOut) --> class(ClassList), classDescription, class(ClassList2).

%% Breaking down generality since we have to generates specific "Representation" for the constraints
classSentence(StackIn, StackOut) --> class(ClassList), [are, in, the, same, room], {addToStack(StackIn, sameRoom(ClassList), StackOut) }.
classSentence(StackIn, StackOut) --> class(ClassList), [have, the, same, teacher], {addToStack(StackIn, sameTeacher(ClassList), StackOut) }.
classSentence(StackIn, StackOut) --> class(ClassList), [are, on, the, same, day], {addToStack(StackIn, sameDay(ClassList), StackOut) }.
classSentence(StackIn, StackOut) --> class(ClassList), [is, in], room(RoomNumber), {addToStack(StackIn, classRoom(ClassList, RoomNumber), StackOut) }.
classSentence(StackIn, StackOut) --> class(ClassList), [is, before], class(ClassList2), {addToStack(StackIn, classBefore(ClassList, ClassList2), StackOut) }.
classSentence(StackIn, StackOut) --> class(ClassList), [is, after], class(ClassList2), {addToStack(StackIn, classAfter(ClassList, ClassList2), StackOut) }.
classSentence(StackIn, StackOut) --> class(ClassList), [has], students(StudentsNumber), {addToStack(StackIn, classStudents(ClassList, StudentsNumber), StackOut) }.

roomSentence(StackIn, StackOut) --> room(RoomNumber), roomDescription, students(StudentsNumber), { addToStack(StackIn, seats(RoomName, StudentsNumber), StackOut) }.

%% emptyStack(X)
%
% emptyStack/1 Creates a new empty stack
%
% @param X The new stack
emptyStack(X) :- X = [].

%% addToStack(Stack, Representation, Stackout)
%
% addToStack/3 Add representation goal to the stack
%
% @param Stack Current Stack
% @param Representation The representation that should be pushed to the Stack
% @param StackOut Updated Stack
addToStack(Stack, Representation, StackOut) :-
	StackOut = [Representation|Stack].

%% updateStack(...)
%
% updateStack/3 add a single new class to the prof representation when "he-she also" is found
% throws error 	when previous sentence is not teacher
% 				when class is already in stack of teacher representation
%
updateStack([Teacher|Stack], ClassList, StackOut) :- 
	\+ Teacher = teacher(_,_),
	throw("Error : Previous sentence should be teacher when he/she is used !").
updateStack([teacher(TeacherName, ClassList)|Stack], NewClass, StackOut) :- 
	Teacher = teacher(_,_),
	member(NewClass, ClassList),
	throw("Error : he/she teacher new class is already in stack !").
updateStack([teacher(TeacherName, ClassList)|Stack], NewClass, [teacher(TeacherName, NewClassList)|Stack]) :- 
	Teacher = teacher(_,_),
	\+ member(NewClass, ClassList),
	append(ClassList, [NewClass], NewClassList).


% ----- CONSTANT VALUES -----
% 10 teacher --> list lenght(ListTeacher, 10)

room(1, 35).
room(2, 60).
room(3, 100).

class(1, 30).
class(2, 35).
class(3, 40).
class(4, 50).
class(5, 10).

%timetable(Data) :-
%	length(TeacherList, 10),
%
%	teacher(...),
%	class(...),
%	room(...)
%


% --
% -- TEST
% --
:- begin_tests(base).

test(prof) :-
	phrase(teacher(smith), [prof, smith]),
	phrase(teacherSuppl, [he, also]).

test(teaching) :-
	phrase(teacherSentence(_,_), [prof, rob, teaches, class, c1]).

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
	phrase(teacherSentence(Stack,[teacher(steve, [c1,c2])]), [prof, steve, teaches, classes, c1, and, c2]),
	phrase(teacherSentence([teacher(steve, [c1,c2])] ,[teacher(steve, [c1,c2,c3])]), [he, also, teaches, class, c3]).

test(stackRoom) :-
	emptyStack(Stack),
	phrase(roomSentence(Stack, [seats(102, 100)]), [room, 102, seats, 100, students]).

test(stackClass) :-
	emptyStack(Stack),
	phrase(classSentence(Stack, [sameRoom([c1,c2,c3])]), [classes, c1, c2, and, c3, are, in, the, same, room]),
	phrase(classSentence(Stack, [sameTeacher([c1,c2])]), [classes, c1, and, c2, have, the, same, teacher]),
	phrase(classSentence(Stack, [sameDay([c1,c2])]), [classes, c1, and, c2, are, on, the, same, day]),
	phrase(classSentence(Stack, [classRoom([c1], 102)]), [class, c1, is, in, room, 102]),
	phrase(classSentence(Stack, [classStudents([c1], 45)]), [class, c1, has, 45, students]),
	phrase(classSentence(Stack, [classAfter([c1], [c2])]), [class, c1, is, after, class, c2]).









