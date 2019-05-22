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
% 8) 	: when pushed, you check in STACK if already exists
% -----

% { class(NAME, DAY, HOUR, TEACHER, SEATS) Class(NAM2)}
% predicate :- class C1 is before class c2
% 	class(NAME, DAY, HOUR, TEACHER, SEATS)
% 	CLASS #< CLASS2
% 	CLass(NAME, SEATS)

% 	push CLASS, CLASS2 

% 	PUSH 

%% ------ QUESTIONS LIST ------
% 1) Split class sentence in order to push to stack the right goal ?
% -> yes
% 2) there is no information about class duration, how are we suppose to give 2 hours of prepration time to teacher then ?
% ->
% 3) Teacher should teach each class every week, I suppose we re talking about HIS classes and not all.
% ->
% 4) est ce que je peux créer les contraintes au fur et a mesure et non pas a lafin. du genre quand je push isbefore() je crée mes contraintes la et je push dans le stack le full clas(*,*,...) directement
% ->
% 5) comment attribuer un numéro à class/teacher qui fit ceux qui existe mais sans essayer dautre numéro

% --- VENDREDI 24 ---
% 6) pour studentnumber: Nbr = X ou Nbr #= X
% 7) crée contraintes au fur et a mesure : essaye dajouter contraintes pendant parsing. Faut il supprimer mon goal de la liste ou je peux modifier la contrainte sans supprimer ?

% 7.1) ou à la fin en créant une liste de 5 elem C1 C2 C3 C4 C5 et lire la liste de goals et completer tout

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
%teacherSentence(StackIn, StackOut) --> teacher(Name), teacherDescription, class(ClassList), { addToStack(StackIn, teacher(Name, ClassList), StackOut) }.
teacherSentence(StackIn, StackOut) --> teacher(Name), teacherDescription, class(ClassList), { addTeacherToStack(StackIn, teacher(Name, ClassList), StackOut) }.

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
classSentence(StackIn, StackOut) --> class(ClassList), [is, before], class(ClassList2), {addClassToStack(StackIn, classBefore(ClassList, ClassList2), StackOut) }.
classSentence(StackIn, StackOut) --> class(ClassList), [is, after], class(ClassList2), {addToStack(StackIn, classAfter(ClassList, ClassList2), StackOut) }.
classSentence(StackIn, StackOut) --> class(ClassList), [has], students(StudentsNumber), {addClassToStack(StackIn, classStudents(ClassList, StudentsNumber), StackOut) }.

%roomSentence(StackIn, StackOut) --> room(RoomNumber), roomDescription, students(StudentsNumber), { addToStack(StackIn, seats(RoomName, StudentsNumber), StackOut) }.
roomSentence(StackIn, StackOut) --> room(RoomName), roomDescription, students(StudentsNumber), { addRoomToStack(StackIn, seats(RoomName, StudentsNumber), StackOut) }.

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

%% addTeacherToStack(Stack, Representation, StackOut)
%
%
%
addTeacherToStack(Stack, teacher(TeacherName, ClassList), StackOut) :-
	teacher(TeacherNumber),
	StackOut = [teacher(TeacherName, TeacherNumber, ClassList)|Stack].


% class : classIn([classes], room)
% class : teacherBy([classes], prof)
% class : before(class1, class2)
% class : after(class1, class2)
% class : sameDay([classes])
% class : sameTeacher()

addClassToStack(Stack, classBefore([ClassNameBefore], [ClassNameAfter]), StackOut) :-
	\+ member(class(ClassNameBefore, _, _, _, _, _, _), Stack),
	\+ member(class(ClassNameAfter, _, _, _, _, _, _), Stack),
	%class(ClassNumber, StudentsNumber), %TODO maybe at the end to unify with studenNumber
	DayBefore in 1..5,
	DayAfter in 1..5,
	HourBefore in 1..5, 
	HourAfter in 1..5,
	( DayBefore #= DayAfter #==> HourBefore #< HourAfter ) #/\ ( DayBefore #\= DayAfter #==> DayBefore #< DayAfter ),
	%( DayBefore #= DayAfter #=> HourBefore #< HourAfter ),
	StackOut = [class(ClassNameBefore, ClassNumberBefore, TeachbyBefore, RoomBefore, DayBefore, HourBefore, StudentsNumberBefore), class(ClassNameAfter, ClassNumberAfter, TeachbyAfter, RoomAfter, DayAfter, HourAfter, StudentsNumberAfter) | Stack].


%% addClassToStack(Stack, Representation, StackOut)
%
% addClassToStack/3
%
% @class representation : class(ClassName, ClassNumber, Teachby, Room, Day, Hour, NumberStudents)
%
addClassToStack(Stack, classStudents([ClassName], StudentsNumber), StackOut) :-
	select(class(ClassName, ClassNumber, Teachby, Room, Day, Hour, _), Stack, ListOut),
	class(ClassNumber, StudentsNumber),
	StackOut = [class(ClassName, ClassNumber, Teachby, Room, Day, Hour, StudentsNumber) | ListOut].
addClassToStack(Stack, classStudents([ClassName], StudentsNumber), StackOut) :-
	\+ select(class(ClassName, _, _, _, _, _, _), Stack, ListOut),
	class(ClassNumber, StudentsNumber),
	StackOut = [class(ClassName, ClassNumber, Teachby, Room, Day, Hour, StudentsNumber) | Stack].


%TODO: class after is just opposite of classBefore
%TODO: No need to use select, you can modify VAR inside directly
%TODO: #==> not #=>



%TODO: use select & member to get and remove from list of objects
%Donc lidee cest de recup le goal envoyé par le parser et dajouter lobject class en fonction + les contraintes
% en verifiant si la class existe déjà dans la liste ou pas.
	

%% addRoomToStack(Stack, Representation, StackOut)
%
%
%
addRoomToStack(Stack, seats(RoomName, StudentsNumber), StackOut) :-
	room(RoomNumber, StudentsNumber),
	StackOut = [room(RoomName, RoomNumber, StudentsNumber)|Stack].


%% updateStack(...)
%
% updateStack/3 add a single new class to the prof representation when "he-she also" is found
% throws error 	when previous sentence is not teacher
% 				when class is already in stack of teacher representation
%
updateStack([Teacher|Stack], ClassList, StackOut) :- 
	\+ Teacher = teacher(_,_,_),
	throw("Error : Previous sentence should be teacher when he/she is used !").
updateStack([teacher(TeacherName, _, ClassList)|Stack], NewClass, StackOut) :- 
	Teacher = teacher(_,_,_),
	member(NewClass, ClassList),
	throw("Error : he/she teacher new class is already in stack !").
updateStack([teacher(TeacherName, TeacherNumber, ClassList)|Stack], NewClass, [teacher(TeacherName, TeacherNumber, NewClassList)|Stack]) :- 
	Teacher = teacher(_,_,_),
	\+ member(NewClass, ClassList),
	append(ClassList, [NewClass], NewClassList).


% ----- CONSTANT VALUES -----
% 10 teacher --> list lenght(ListTeacher, 10)

%Should block here
teacher(1).
teacher(2).
teacher(3).
teacher(4).
teacher(5).
teacher(6).
teacher(7).
teacher(8).
teacher(9).
teacher(10).

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
	emptyStack(X),
	phrase(classSentence(_,_), [class, c1, is, in, room, 102]),
	phrase(classSentence(X,_), [class, c1, is, before, class, c2]),
	phrase(classSentence(_,_), [classes, c1, and, c2, are, in, the, same, room]),
	phrase(classSentence(_,_), [classes, c1, c2, and, c3, have, the, same, teacher]).

test(room) :-
	phrase(roomSentence(_,_), [room, 102, seats, 100, students]).

test(stackTeacher) :- 
	emptyStack(Stack),
	phrase(teacherSentence(Stack,[teacher(rob, _, [c1])]), [prof, rob, teaches, class, c1]),	
	phrase(teacherSentence(Stack,[teacher(steve, _, [c1,c2])]), [prof, steve, teaches, classes, c1, and, c2]),
	phrase(teacherSentence([teacher(steve, _, [c1,c2])], [teacher(steve, _, [c1,c2,c3])]), [he, also, teaches, class, c3]).

test(stackRoom) :-
	emptyStack(Stack),
	%phrase(roomSentence(Stack, [seats(102, 100)]), [room, 102, seats, 100, students]).
	phrase(roomSentence(Stack, [room(102, 3, 100)]), [room, 102, seats, 100, students]).

% test(stackClass) :-
% 	emptyStack(Stack),
% 	phrase(classSentence(Stack, [sameRoom([c1,c2,c3])]), [classes, c1, c2, and, c3, are, in, the, same, room]),
% 	phrase(classSentence(Stack, [sameTeacher([c1,c2])]), [classes, c1, and, c2, have, the, same, teacher]),
% 	phrase(classSentence(Stack, [sameDay([c1,c2])]), [classes, c1, and, c2, are, on, the, same, day]),
% 	phrase(classSentence(Stack, [classRoom([c1], 102)]), [class, c1, is, in, room, 102]),
% 	phrase(classSentence([class(c1, _, _, _, _, _, _)], [class(c1, 1, _, _, _, _, 30)]), [class, c1, has, 30, students]),
% 	phrase(classSentence(Stack, [classAfter([c1], [c2])]), [class, c1, is, after, class, c2]).

 test(stackClass) :-
 	emptyStack(Stack),
 	phrase(classSentence([class(c1, _, _, _, _, _, _)], [class(c1, 1, _, _, _, _, 30)]), [class, c1, has, 30, students]),
	phrase(classSentence(Stack, [classAfter([c1], [c2])]), [class, c1, is, before, class, c2]).








