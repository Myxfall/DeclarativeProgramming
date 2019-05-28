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
% 6) pour studentnumber: Nbr = X ou Nbr #= X, Si je sais déjà que ca va être ca, est ce que je dois quand même utiliser #= au cas ou
% X) crée contraintes au fur et a mesure : essaye dajouter contraintes pendant parsing. Faut il supprimer mon goal de la liste ou je peux modifier la contrainte sans supprimer ?
% 7.1) ou à la fin en créant une liste de 5 elem C1 C2 C3 C4 C5 et lire la liste de goals et completer tout
% 8) le parser ne crée pas doffice toutes les classes/room, du coup comment creeer les restantes a la fin? ou ne pas les crer alors ?
% 9) probleme dassociation name/number, obligé de le faire à la fin.
% 10) how to block(-,-), does not work

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

% !!!!! IMPORTANT !!!!!!! Pour les association nombres/name, le faire à la fin, et cest student number qui définit ca.
% peut être que "VAR in 1..10" doit être à la fin pour chaque classes

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
classSentence(StackIn, StackOut) --> class(ClassList), [are, in, the, same, room], {addClassToStack(StackIn, sameRoom(ClassList), StackOut) }.
classSentence(StackIn, StackOut) --> class(ClassList), [have, the, same, teacher], {addClassToStack(StackIn, sameTeacher(ClassList), StackOut) }.
classSentence(StackIn, StackOut) --> class(ClassList), [are, on, the, same, day], {addClassToStack(StackIn, sameDay(ClassList), StackOut) }.
classSentence(StackIn, StackOut) --> class(ClassList), [is, in], room(RoomNumber), {addClassToStack(StackIn, classRoom(ClassList, RoomNumber), StackOut) }.
classSentence(StackIn, StackOut) --> class(ClassList), [is, before], class(ClassList2), {addClassToStack(StackIn, classBefore(ClassList, ClassList2), StackOut) }.
classSentence(StackIn, StackOut) --> class(ClassList), [is, after], class(ClassList2), {addClassToStack(StackIn, classBefore(ClassList2, ClassList), StackOut) }. % Class after is just the opposite of class Before
classSentence(StackIn, StackOut) --> class(ClassList), [has], students(StudentsNumber), {addClassToStack(StackIn, classStudents(ClassList, StudentsNumber), StackOut) }.

%roomSentence(StackIn, StackOut) --> room(RoomNumber), roomDescription, students(StudentsNumber), { addToStack(StackIn, seats(RoomName, StudentsNumber), StackOut) }.
roomSentence(StackIn, StackOut) --> room(RoomName), roomDescription, students(StudentsNumber), { addRoomToStack(StackIn, seats(RoomName, StudentsNumber), StackOut) }.

fullstop --> [fullstop].
fullstop --> [.].

line(StackIn, StackOut) --> teacherSentence(StackIn, StackOut).
line(StackIn, StackOut) --> classSentence(StackIn, StackOut).
line(StackIn, StackOut) --> roomSentence(StackIn, StackOut).

%% emptyStack(X)
%
% emptyStack/1 Creates a new empty stack
%
% @param X The new stack
%
emptyStack(X) :- X = [].

%% addToStack(Stack, Representation, Stackout)
%
% addToStack/3 Add representation goal to the stack
%
% @param Stack Current Stack
% @param Representation The representation that should be pushed to the Stack
% @param StackOut Updated Stack
%
addToStack(Stack, Representation, StackOut) :-
	StackOut = [Representation|Stack].

%% addTeacherToStack(Stack, Representation, StackOut)
%
% addTeacherToStack/3 Push the teacher representation into the stack
%
% @param Stack The current stack with all the representations
% @param teacher(Name, Number) The goal it receives, will add constraintes depending on the goal form
% @parem StackOut The Stack out with the new representation pushed into
%
addTeacherToStack(Stack, teacher(TeacherName, ClassList), StackOut) :-
	%teacher(TeacherNumber),
	addClassFromTeacher(ClassList, NewClasses, Stack),
	append(NewClasses, Stack, StackTMP),
	StackOut = [teacher(TeacherName, TeacherNumber, ClassList)|StackTMP].

addClassFromTeacher([], [], Stack).
addClassFromTeacher([Class|ClassList], Classes, Stack) :-
	member(class(Class,_,_,_,_,_,_), Stack),
	addClassFromTeacher(ClassList, Classes, Stack).
addClassFromTeacher([Class|ClassList], [NewClass|Classes], Stack) :-
	\+ member(class(Class,_,_,_,_,_,_), Stack),
	NewClass = class(Class,_,_,_,_,_,_),
	addClassFromTeacher(ClassList, Classes, Stack).


%% addClassToStack(Stack, Representation, StackOut)
%
% addClassToStack/3 Add the class representation into the stack depending on the goal it receives
%
% @param Stack The current stack with all the representations
% @param goal(X,Y,***) The goal it receives, will add constraintes depending on the goal form
% @parem StackOut The Stack out with the new representation pushed into
% @class representation : class(ClassName, ClassNumber, Teachby, Room, Day, Hour, NumberStudents)
%
addClassToStack(Stack, classBefore([ClassNameBefore], [ClassNameAfter]), StackOut) :-
	\+ member(class(ClassNameBefore, _, _, _, _, _, _), Stack),
	\+ member(class(ClassNameAfter, _, _, _, _, _, _), Stack),
	%class(ClassNumberBefore, StudentsNumberBefore), %TODO maybe at the end to unify with studenNumber
	%class(ClassNumberAfter, StudentsNumberAfter),
	%DayBefore in 1..5,
	%check day&hour different
	%DayAfter in 1..5,
	%check day&hour different
	%HourBefore in 1..5,
	%check day&hour different 
	%HourAfter in 1..5,
	%check day&hour different
	( DayBefore #= DayAfter #==> HourBefore #< HourAfter ) #/\ ( DayBefore #\= DayAfter #==> DayBefore #< DayAfter ),
	StackOut = [class(ClassNameBefore, ClassNumberBefore, TeachbyBefore, RoomBefore, DayBefore, HourBefore, StudentsNumberBefore), class(ClassNameAfter, ClassNumberAfter, TeachbyAfter, RoomAfter, DayAfter, HourAfter, StudentsNumberAfter) | Stack].
% C1 is in Stack -> Push C2
addClassToStack(Stack, classBefore([ClassNameBefore], [ClassNameAfter]), StackOut) :-
	member(class(ClassNameBefore, _, _, _, DayBefore, HourBefore, _), Stack),
	\+ member(class(ClassNameAfter, _, _, _, _, _, _), Stack),
	%DayAfter in 1..5,
	%check day&hour different
	%HourAfter in 1..5,
	%check day&hour different
	( DayBefore #= DayAfter #==> HourBefore #< HourAfter ) #/\ ( DayBefore #\= DayAfter #==> DayBefore #< DayAfter ),
	StackOut = [class(ClassNameAfter, ClassNumberAfter, TeachbyAfter, RoomAfter, DayAfter, HourAfter, StudentsNumberAfter) | Stack].
% C2 is in Stack -> Push C1
addClassToStack(Stack, classBefore([ClassNameBefore], [ClassNameAfter]), StackOut) :-
	\+ member(class(ClassNameBefore, _, _, _, _, _, _), Stack),
	member(class(ClassNameAfter, _, _, _, DayAfter, HourAfter, _), Stack),
	%DayBefore in 1..5,
	%check day&hour different
	%HourBefore in 1..5,
	%check day&hour different
	( DayBefore #= DayAfter #==> HourBefore #< HourAfter ) #/\ ( DayBefore #\= DayAfter #==> DayBefore #< DayAfter ),
	StackOut = [class(ClassNameBefore, ClassNumberBefore, TeachbyBefore, RoomBefore, DayBefore, HourBefore, StudentsNumberBefore) | Stack].
% Both classes are already in stack, just adding constraintes
addClassToStack(Stack, classBefore([ClassNameBefore], [ClassNameAfter]), Stack) :-
	member(class(ClassNameBefore, _, _, _, DayBefore, HourBefore, _), Stack),
	member(class(ClassNameAfter, _, _, _, DayAfter, HourAfter, _), Stack),
	%DayBefore in 1..5,
	%check day&hour different
	%DayAfter in 1..5,
	%check day&hour different
	%HourBefore in 1..5, 
	%HourAfter in 1..5,
	( DayBefore #= DayAfter #==> HourBefore #< HourAfter ) #/\ ( DayBefore #\= DayAfter #==> DayBefore #< DayAfter ).

% Constraintes on student number
addClassToStack(Stack, classStudents([ClassName], StudentsNumber), StackOut) :-
	member(class(ClassName, ClassNumber, Teachby, Room, Day, Hour, StudentsNumber), Stack),
	%class(ClassNumber, Number),
	StudentsNumber #= Number.
	%StackOut = [class(ClassName, ClassNumber, Teachby, Room, Day, Hour, StudentsNumber) | ListOut].
addClassToStack(Stack, classStudents([ClassName], StudentsNumber), StackOut) :-
	\+ member(class(ClassName, _, _, _, _, _, _), Stack),
	%class(ClassNumber, StudentsNumber), 
	StackOut = [class(ClassName, ClassNumber, Teachby, Room, Day, Hour, StudentsNumber) | Stack].

% Constraintes on class room
addClassToStack(Stack, classRoom([ClassName], RoomNumber), StackOut) :-
	\+ member(class(ClassName, _, _, Room, _, _, _), Stack),
	Room #= RoomNumber, % Should be name or associated name of ROOM ?
	StackOut = [class(ClassName, ClassNumber, Teachby, Room, Day, Hour, StudentsNumber) | Stack].
addClassToStack(Stack, classRoom([ClassName], RoomNumber), StackOut) :-
	member(class(ClassName, _, _, Room, _, _, _), Stack),
	Room #= RoomNumber. % Should be name or associated name of ROOM ?

% Constraintes on same room classes
addClassToStack(Stack, sameRoom(ClassList), StackOut) :-
	goThroughSameRoom(ClassList, NewClasses, [Room1, Room2], Stack),
	Room1 #= Room2,
	%StackOut = [NewClasses | Stack].
	append(NewClasses, Stack, StackOut).
	%StackOut = [Room1, Room2 | Stack].
addClassToStack(Stack, sameRoom(ClassList), StackOut) :-
	goThroughSameRoom(ClassList, NewClasses, [Room1, Room2, Room3], Stack),
	Room1 #= Room2,
	Room2 #= Room3,
	%StackOut = [NewClasses | Stack].
	append(NewClasses, Stack, StackOut).

% Used to go through a list of Class and add to an accumulateur. This allows me to handle in a better way having several classes
% and prevent me to create 9 differents predicates, does c1 exists in stack but c2 and c3 not, etc, etc...
goThroughSameRoom([], [], [], Stack).
goThroughSameRoom([ClassName|RestClasses], [NewClass|ClassesOut], [NewRoom|RoomOut], Stack) :-
	\+ member(class(ClassName, _, _, _, _, _, _), Stack),
	NewClass = class(ClassName, ClassNumber, Teachby, Room, Day, Hour, StudentsNumber),
	NewRoom = Room,
	goThroughSameRoom(RestClasses, ClassesOut, RoomOut, Stack).
goThroughSameRoom([ClassName|RestClasses], ClassesOut, [Room|RoomOut], Stack) :-
	member(class(ClassName, _, _, Room, _, _, _), Stack),
	%NewRoom = Room,
	goThroughSameRoom(RestClasses, ClassesOut, RoomOut, Stack).

% Constraintes on same day classes
addClassToStack(Stack, sameDay(ClassList), StackOut) :-
	goThroughSameDay(ClassList, NewClasses, [Day1, Day2], Stack),
	Day1 #= Day2,
	%StackOut = [NewClasses | Stack].
	append(NewClasses, Stack, StackOut).
addClassToStack(Stack, sameDay(ClassList), StackOut) :-
	goThroughSameDay(ClassList, NewClasses, [Day1, Day2, Day3], Stack),
	Day1 #= Day2,
	Day2 #= Day3,
	%StackOut = [NewClasses | Stack].
	append(NewClasses, Stack, StackOut).
		
goThroughSameDay([], [], [], Stack).
goThroughSameDay([ClassName|RestClasses], [NewClass|ClassesOut], [NewDay|RoomOut], Stack) :-
	\+ member(class(ClassName, _, _, _, _, _, _), Stack),
	NewClass = class(ClassName, ClassNumber, Teachby, Room, Day, Hour, StudentsNumber),
	NewDay = Day,
	goThroughSameDay(RestClasses, ClassesOut, RoomOut, Stack).
goThroughSameDay([ClassName|RestClasses], ClassesOut, [NewDay|RoomOut], Stack) :-
	member(class(ClassName, _, _, _, Day, _, _), Stack),
	NewDay = Day,
	goThroughSameDay(RestClasses, ClassesOut, RoomOut, Stack).

addClassToStack(Stack, sameTeacher(ClassList), StackOut) :-
	goThroughSameTeacher(ClassList, NewClasses, [Teacher1, Teacher2], Stack),
	Teacher1 #= Teacher2,
	append(NewClasses, Stack, StackOut).
addClassToStack(Stack, sameTeacher(ClassList), StackOut) :-
	goThroughSameTeacher(ClassList, NewClasses, [Teacher1, Teacher2, Teacher3], Stack),
	Teacher1 #= Teacher2,
	Teacher2 #= Teacher3,
	append(NewClasses, Stack, StackOut).

goThroughSameTeacher([], [], [], Stack).
goThroughSameTeacher([ClassName|RestClasses], [NewClasses|ClassesOut], [Teachby|TeacherOut], Stack) :-
	\+ member(class(ClassName, _, _, _, _, _, _), Stack),
	NewClasses = class(ClassName, ClassNumber, Teachby, Room, Day, Hour, StudentsNumber),
	goThroughSameTeacher(RestClasses, ClassesOut, TeacherOut, Stack).
goThroughSameTeacher([ClassName|RestClasses], ClassesOut, [Teachby|TeacherOut], Stack) :-
	member(class(ClassName, _, Teachby, _, _, _, _), Stack),
	goThroughSameTeacher(RestClasses, ClassesOut, TeacherOut, Stack).

%% addRoomToStack(Stack, Representation, StackOut)
%
% addRoomToStack/3 Push the Room representation into the stack
%
% @param Stack The current stack with all the representations
% @param room(Name, StudentsNumbers) The goal it receives, will add constraintes depending on the goal form
% @parem StackOut The Stack out with the new representation pushed into
%
addRoomToStack(Stack, seats(RoomName, StudentsNumber), StackOut) :-
	room(RoomNumber, StudentsNumber),
	StdNbr #= StudentsNumber,
	StackOut = [room(RoomName, RoomNumber, StdNbr)|Stack].

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


% --
% -- FINAL CONSTRAINTES
% --

%test2(D), splitFullstop(D, Out), foldl(parse, Out, [], Out2), buildTeacherList(Out2, Teachers), buildClassList(Out2, Classes), buildRoomList(Out2, Rooms), length(T, 3), teacherConstraintes(T, Teachers, Out3), all_different(T).

timeTable(Data, TimeTable) :-
	parseText(Data, TimeTable),

	% Teachers
	length(TeacherNumber, 3),
	all_different(TeacherNumber),
	buildTeacherList(TimeTable, TeachersList),
	teacherConstraintes(TeacherNumber, TeachersList, TimeTable),

	% Rooms
	length(RoomNumber, 3),
	all_different(RoomNumber),
	buildRoomList(TimeTable, RoomsList),
	roomConstraintes(RoomNumber, RoomsList, TimeTable),

	% Classes
	length(ClassNumber, 3),
	all_different(ClassNumber),
	buildClassList(TimeTable, ClassesList),
	classConstraintes(ClassNumber, ClassesList, TimeTable, Variables),
	labeling([ffc], Variables).

buildTeacherList([], []).
buildTeacherList([Teacher|Stack], [teacher(TeacherName, TeacherNumber, ClassList)|TeachersList]) :-
	Teacher = teacher(TeacherName, TeacherNumber, ClassList),
	buildTeacherList(Stack, TeachersList).
buildTeacherList([Elem|Stack], TeacherList) :-
	\+ Elem = teacher(_,_,_), 
	buildTeacherList(Stack, TeacherList). 

teacherConstraintes([],[], _).
teacherConstraintes([Teacher|Teachers], [teacher(TeacherName, Teacher, ClassList)|TeachersList], Stack) :-
	teacher(Teacher),
	goThroughTeachBy(ClassList, Teacher, Stack),
	teacherConstraintes(Teachers, TeachersList, Stack).

goThroughTeachBy([], _, _).
goThroughTeachBy([Class|ClassList], TeacherNumber, Stack) :-
	member(class(Class, _,Teacher,_,_,_,_), Stack),
	Teacher #= TeacherNumber,
	goThroughTeachBy(ClassList, TeacherNumber, Stack).
goThroughTeachBy([Class|ClassList], TeacherNumber, Stack) :-
	\+ member(class(Class, _,_,_,_,_,_), Stack),
	goThroughTeachBy(ClassList, TeacherNumber, Stack).

buildRoomList([], []).
buildRoomList([Room|Rooms], [Room|RoomsList]) :-
	Room = room(RoomName, RoomNumber, Students),
	buildRoomList(Rooms, RoomsList).
buildRoomList([Room|Rooms], RoomsList) :-
	\+ Room = room(_,_,_), 
	buildRoomList(Rooms, RoomsList).

roomConstraintes([], [], _).
roomConstraintes([RoomNumber|RoomsNumbers], [room(RoomName, RoomNumber, Students)|RoomsList], TimeTable) :-
	room(RoomNumber, Students),
	roomConstraintes(RoomsNumbers, RoomsList, TimeTable).

buildClassList([], []).
buildClassList([Class|Stack], [Class|ClassesList]) :-
	Class = class(_, _, _, _, _, _, _),
	buildClassList(Stack, ClassesList).
buildClassList([Elem|Stack], ClassesList) :-
	\+ Elem = class(_,_,_,_,_,_,_), 
	buildClassList(Stack, ClassesList). 

classConstraintes([], [], _, []).
classConstraintes([ClassNumber|ClassNumbers], [class(ClassName, ClassNumber, Teachby, Room, Day, Hour, NumberStudents)|ClassesList], TimeTable, [Day, Hour|Variables]) :-
	class(ClassNumber, NumberStudents),
	Day in 1..5,
	Hour in 1..5,
	scheduleClassComparison(class(ClassName, ClassNumber, Teachby, Room, Day, Hour, NumberStudents), ClassesList),
	member(room(_,Room, SizeRoom), TimeTable),
	NumberStudents #< SizeRoom,
	classConstraintes(ClassNumbers, ClassesList, TimeTable, Variables).

%% scheduleClassComparison(Class, ClassList)
%
% scheduleClassComparison/2 Compare one given class to the other classes in order to prevent classes in the same room at the same moment
%
% @param Class The Class that is going to be compare to the other classes in the list
% @param ClassList The Class list that is going to be compared with
%
scheduleClassComparison(_, []).
scheduleClassComparison(class(_, _, _, Room1, Day1, Hour1, _), [class(_, _, _, Room2, Day2, Hour2, _)|Tail]):-
	( Day1 #= Day2 #==> Hour1 #= Hour2 #==> Room1 #\= Room2),
	scheduleClassComparison(class(_, _, _, _, Day1, Hour1, _), Tail).

% --
% -- PARSING
% --

%% splitFullstop(In, Out)
%
% splitFullstop/2 Receives a List of sentences and created a list of list of these sentences
splitFullstop([], []).
splitFullstop([fullstop|T], [[]|T2]) :-
    splitFullstop(T, T2).
splitFullstop([H|T], [[H|T2]|T3]) :-
    dif(H, stop),
    splitFullstop(T, [T2|T3]).

%% parse(Line, StackIn, StackOut)
%
% parse/3
%
% @param Line The current line being parsed
% @param StackIn Current Stack with the goal representations
% @param StackOut Stack being filled in
parse(Line, StackIn, StackOut) :-
  phrase(line(StackIn, StackOut), Line).

%% parseText(Text, StackOut)
%
% parseText/2 Parse the text it receives and creates a list of sentences.
%
% @param Text The input text, in the format of [jones, teaches, something, fullstop, ...]
% @param StackOut, The list of sentences created by the predicate
%
parseText(Text, StackOut) :-
  emptyStack(Stack),
  splitFullstop(Text, ListSentences),
  foldl(parse, ListSentences, Stack, StackOut).


% --
% -- PRINTING
% --

print_timetable(TimeTable) :-
	writeln("\n----- Start of TimeTable -----\n"),

	writeln("----- Rooms -----"),
	buildRoomList(TimeTable, RoomList),
	printRooms(RoomList),

	writeln("----- Teacher -----"),
	buildTeacherList(TimeTable, TeacherList),
	printTeachers(TeacherList),

	writeln("----- Classes -----"),
	buildClassList(TimeTable, ClassList),
	%sort(5, @=<, ClassList, SortedClasses),
	
	sortByDay(1, ClassList, Mondays),
	sortByDay(2, ClassList, Tuesdays),
	sortByDay(3, ClassList, Wednesdays),
	sortByDay(4, ClassList, Thursdays),
	sortByDay(5, ClassList, Fridays),

	sort(6, @=<, Mondays, SortedMondays),
	sort(6, @=<, Tuesdays, SortedTuesdays),
	sort(6, @=<, Wednesdays, SortedWednesdays),
	sort(6, @=<, Thursdays, SortedThursdays),
	sort(6, @=<, Fridays, SortedFridays),

	writeln("Monday : "),
	printClasses(SortedMondays),
	writeln("Tuesday : "),
	printClasses(SortedTuesdays),
	writeln("Wednesday : "),
	printClasses(SortedWednesdays),
	writeln("Thursday : "),
	printClasses(SortedThursdays),
	writeln("Friday : "),
	printClasses(SortedFridays),


	writeln("\n----- END OF TIMETABLE -----\n").

printRooms([]).
printRooms([room(RoomName, RoomNumber, RoomSize)|TimeTable]) :-
	write("Room "),
	write_term(RoomName, []),
	write(" with "),
	write_term(RoomSize, []),
	write(" seats"),
  	writeln(""),
	printRooms(TimeTable).

printTeachers([]).
printTeachers([teacher(TeacherName, TeacherNbr, ClassList)|TimeTable]) :-
	write("Teacher "),
	write_term(TeacherName, []),
	write(" teaches the class(es) : "),
	printClassesT(ClassList),
	writeln(""),
	printTeachers(TimeTable).

printClassesT([]).
printClassesT([Class|Classes]) :-
	write_term(Class, []),
	write(" "),
	printClassesT(Classes).

printClasses([]).
printClasses([class(ClassName, ClassNumber, Teachby, Room, Day, Hour, NumberStudents)|TimeTable]) :-
	write("Class "),
	write_term(ClassName, []),
	write(" teaches by "), % name of teacher
	write_term(Teachby, []),
	write(" in room "),
	write_term(Room, []),
	printDay(Day, Hour),
	writeln(""),
	printClasses(TimeTable).

sortByDay(_, [], []).
sortByDay(Day, [Class|ClassList], [Class|DayClasses]) :-
	Class = class(_,_,_,_,Day,_,_),
	sortByDay(Day, ClassList, DayClasses).
sortByDay(Day, [Class|ClassList], DayClasses) :-
	\+ Class = class(_,_,_,_,Day,_,_),
	sortByDay(Day, ClassList, DayClasses).

printDay(1, Hour) :-
	write(" given on Monday"),
	printInfosClass(Hour).
printDay(2, Hour) :-
	write(" given on Tuesday"),
	printInfosClass(Hour).
printDay(3, Hour) :-
	write(" given on Wednesday"),
	printInfosClass(Hour).
printDay(4, Hour) :-
	write(" given on Thursday"),
	printInfosClass(Hour).
printDay(5, Hour) :-
	write(" given on Friday"),
	printInfosClass(Hour).

printInfosClass(Hour) :-
	Slot is 8 + Hour,
	write(" at "),
	write_term(Slot, []),
	write(":00").



% ----- CONSTANT VALUES -----
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

test([prof, steve, teaches, class, c1, fullstop,
 prof, rob, teaches, class, c2, fullstop,
 prof, junior, teaches, class, c3, fullstop,
 %he, also, teaches, class, c4, fullstop,
 class, c1, has, 30, students, fullstop,
 class, c2, has, 35, students, fullstop,
 class, c1, is, before, class, c2, fullstop,
 %classes, c1, and, c2, are, on, the, same, day, fullstop,
 %classes, c1, and, c2, are, in, the, same, room, fullstop,
 %classes, c1, and, c2, have, the, same, teacher, fullstop,
 room, 102, seats, 100, students, fullstop,
 room, 202, seats, 35, students, fullstop ]).

% 3 Teachers, 2 Room, 3 Classes
test2([prof, steve, teaches, class, c1, fullstop,
 prof, rob, teaches, class, c2, fullstop,
 prof, junior, teaches, class, c3, fullstop,
 %classes, c3, and, c2, are, in, the, same, room, fullstop,
 %classes, c1, and, c3, are, on, the, same, day, fullstop,
 class, c1, is, before, class, c3, fullstop,
 class, c1, is, in, room, 102, fullstop,
 room, 102, seats, 60, students, fullstop,
 room, 202, seats, 35, students, fullstop,
 room, 302, seats, 100, students, fullstop]).


%bug for same room, add doublons

%bug for same teacher, return false
%Also teach should also add missing class into stack
%Class comparison, not teacher at same time

 
% --
% -- TEST
% --
% class(ClassName, ClassNumber, Teachby, Room, Day, Hour, NumberStudents)

:- begin_tests(base).

test(prof) :-
	phrase(teacher(smith), [prof, smith]),
	phrase(teacherSuppl, [he, also]).

test(teaching) :-
	phrase(teacherSentence(_,_), [prof, rob, teaches, class, c1]).

test(class) :-
	emptyStack(X),
	phrase(classSentence(X,_), [class, c1, is, in, room, 102]),
	phrase(classSentence(X,_), [class, c1, is, before, class, c2]),
	phrase(classSentence(X,_), [classes, c1, and, c2, are, in, the, same, room]),
	phrase(classSentence(X,_), [classes, c1, c2, and, c3, have, the, same, teacher]).

test(room) :-
	phrase(roomSentence(_,_), [room, 102, seats, 100, students]).

test(stackTeacher) :- 
	emptyStack(Stack),
	phrase(teacherSentence(Stack,[teacher(rob, _, [c1]), class(c1,_,_,_,_,_,_)]), [prof, rob, teaches, class, c1]),
	phrase(teacherSentence(Stack,[teacher(steve, _, [c1,c2]), class(c1,_,_,_,_,_,_), class(c2,_,_,_,_,_,_)]), [prof, steve, teaches, classes, c1, and, c2]),
	phrase(teacherSentence([teacher(steve, _, [c1,c2])], [teacher(steve, _, [c1,c2,c3])]), [he, also, teaches, class, c3]).

test(stackRoom) :-
	emptyStack(Stack),
	%phrase(roomSentence(Stack, [seats(102, 100)]), [room, 102, seats, 100, students]).
	phrase(roomSentence(Stack, [room(102, 3, 100)]), [room, 102, seats, 100, students]).

 test(stackClass) :-
	emptyStack(Stack),
	phrase(classSentence([class(c1, _, _, _, _, _, _)], [class(c1, 1, _, _, _, _, 30)]), [class, c1, has, 30, students]),
	phrase(classSentence(Stack, [class(c1, _,_,_,_,_,_), class(c2, _,_,_,_,_,_)]), [class, c1, is, before, class, c2]),
	phrase(classSentence([class(c1, _,_,_,_,_,_)], [class(c2, _,_,_,_,_,_), class(c1, _,_,_,_,_,_)]), [class, c1, is, before, class, c2]),
	phrase(classSentence(Stack, [class(c2,_,_,_,_,_,_), class(c1,_,_,_,_,_,_)]), [class, c1, is, after, class, c2]),
	phrase(classSentence(Stack, [class(c1,_,_,102,0,_,_)]), [class, c1, is, in, room, 102]),
	phrase(classSentence([class(c1,_,_,_,_,_,_)], [class(c1,_,_,102,_,_,_)]), [class, c1, is, in, room, 102]),
	phrase(classSentence(Stack, [class(c1, _, _, Room, _, _, _), class(c2, _, _, Room, _, _, _)]), [classes, c1, and, c2, are, in, the, same, room]),
	phrase(classSentence(Stack, [class(c1, _, _, _, Day, _, _), class(c2, _, _, _, Day, _, _)]), [classes, c1, and, c2, are, on, the, same, day]),
	phrase(classSentence(Stack, [class(c1, _, _, _, Day, _, _), class(c2, _, _, _, Day, _, _), class(c3, _, _, _, Day, _, _)]), [classes, c1, c2, and, c3, are, on, the, same, day]),
	phrase(classSentence([class(c3, _, _, _, Day, _, _)], [class(c1, _, _, _, Day, _, _), class(c2, _, _, _, Day, _, _), class(c3, _, _, _, Day, _, _)]), [classes, c1, and, c2, are, on, the, same, day]),
	phrase(classSentence(Stack, [class(c1, _, _, _, _, _, _), class(c2, _, _, _, _, _, _)]), [classes, c1, and, c2, have, the, same, teacher]).

test(line) :-
	emptyStack(Stack),
	%phrase(line(Stack,[teacher(steve, _, [c1,c2])]), [prof, steve, teaches, classes, c1, and, c2]),
	phrase(line(Stack, [class(c1, _,_,_,_,_,_), class(c2, _,_,_,_,_,_)]), [class, c1, is, before, class, c2]),
	phrase(line(Stack, [room(102, 3, 100)]), [room, 102, seats, 100, students]).


%[room, 102, seats, 100, students, fullstop, prof, steve, teaches, classes, c1, and, c2, fullstop, class, c1, has, 30, students, fullstop, classes, c1, and, c2, are, in, the, same, room, fullstop].
%[prof, steve, teaches, classes, c1, fullstop, prof, rob, teaches, classes, c2, fullstop, prof, junior, teaches, classes, c3, fullstop ]


