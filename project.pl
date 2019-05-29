%['/Users/max/Dropbox/Unif/Master/DeclarativeProgramming/SecondYear/Project/DeclarativeProgramming/project.pl'].

:- use_module(library(clpfd)).

% --
% -- DCG
% --

% Teacher
prof --> [prof].
teacherName(Name) --> [Name].
teacher(Name) --> prof, teacherName(Name).
teacherSuppl --> [he, also].
teacherSuppl --> [she, also].

% Class
classWord --> [class].
classWord --> [classes].
classNumber(Number) --> [Number].
class([ClassOne, ClassTwo, ClassThree]) --> classWord, classNumber(ClassOne), classNumber(ClassTwo), conjonction, classNumber(ClassThree).
class([ClassOne, ClassTwo]) --> classWord, classNumber(ClassOne), conjonction, classNumber(ClassTwo).
class([Number]) --> classWord, classNumber(Number).

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
roomDescription --> [seats].
conjonction --> [and].

% Sentence description
teacherSentence(StackIn, StackOut) --> teacher(Name), teacherDescription, class(ClassList), { addTeacherToStack(StackIn, teacher(Name, ClassList), StackOut) }.

teacherSentence(StackIn, StackOut) --> teacherSuppl, teacherDescription, class([Class]), { updateStack(StackIn, Class, StackOut) }.

% Breaking down generality since we have to generates specific "Representation" for the constraints
classSentence(StackIn, StackOut) --> class(ClassList), [are, in, the, same, room], {addClassToStack(StackIn, sameRoom(ClassList), StackOut) }.
classSentence(StackIn, StackOut) --> class(ClassList), [have, the, same, teacher], {addClassToStack(StackIn, sameTeacher(ClassList), StackOut) }.
classSentence(StackIn, StackOut) --> class(ClassList), [are, on, the, same, day], {addClassToStack(StackIn, sameDay(ClassList), StackOut) }.
classSentence(StackIn, StackOut) --> class(ClassList), [is, in], room(RoomNumber), {addClassToStack(StackIn, classRoom(ClassList, RoomNumber), StackOut) }.
classSentence(StackIn, StackOut) --> class(ClassList), [is, before], class(ClassList2), {addClassToStack(StackIn, classBefore(ClassList, ClassList2), StackOut) }.
classSentence(StackIn, StackOut) --> class(ClassList), [is, after], class(ClassList2), {addClassToStack(StackIn, classBefore(ClassList2, ClassList), StackOut) }. % Class after is just the opposite of class Before
classSentence(StackIn, StackOut) --> class(ClassList), [has], students(StudentsNumber), {addClassToStack(StackIn, classStudents(ClassList, StudentsNumber), StackOut) }.

roomSentence(StackIn, StackOut) --> room(RoomName), roomDescription, students(StudentsNumber), { addRoomToStack(StackIn, seats(RoomName, StudentsNumber), StackOut) }.

fullstop --> [fullstop].
fullstop --> [.].

line(StackIn, StackOut) --> teacherSentence(StackIn, StackOut).
line(StackIn, StackOut) --> classSentence(StackIn, StackOut).
line(StackIn, StackOut) --> roomSentence(StackIn, StackOut).

% --
% -- Stack Filling
% --

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
	StackOut = [teacher(TeacherName, _, ClassList)|Stack].

%% addClassToStack(Stack, Representation, StackOut)
%
% addClassToStack/3 Add the class representation into the stack depending on the goal it receives
%
% @param Stack The current stack with all the representations
% @param goal(X,Y,***) The goal it receives, will add constraintes depending on the goal form
% @parem StackOut The Stack out with the new representation pushed into
% @class representation : class(ClassName, ClassNumber, Teachby, Room, Day, Hour, NumberStudents)
% @constrainte : If Day of two classes are equals, then Hour1 should be smaller than Hour2. If days are not equals then day1 should be smaller. Ensures that the first class is taught before
%
addClassToStack(Stack, classBefore([ClassNameBefore], [ClassNameAfter]), StackOut) :-
	\+ member(class(ClassNameBefore, _, _, _, _, _, _), Stack),
	\+ member(class(ClassNameAfter, _, _, _, _, _, _), Stack),
	( DayBefore #= DayAfter #==> HourBefore #< HourAfter ) #/\ ( DayBefore #\= DayAfter #==> DayBefore #< DayAfter ),
	StackOut = [class(ClassNameBefore, _, _, _, DayBefore, HourBefore, _), class(ClassNameAfter, _, _, _, DayAfter, HourAfter, _) | Stack].
% C1 is in Stack -> Push C2
addClassToStack(Stack, classBefore([ClassNameBefore], [ClassNameAfter]), StackOut) :-
	member(class(ClassNameBefore, _, _, _, DayBefore, HourBefore, _), Stack),
	\+ member(class(ClassNameAfter, _, _, _, _, _, _), Stack),
	( DayBefore #= DayAfter #==> HourBefore #< HourAfter ) #/\ ( DayBefore #\= DayAfter #==> DayBefore #< DayAfter ),
	StackOut = [class(ClassNameAfter, _, _, _, DayAfter, HourAfter, _) | Stack].
% C2 is in Stack -> Push C1
addClassToStack(Stack, classBefore([ClassNameBefore], [ClassNameAfter]), StackOut) :-
	\+ member(class(ClassNameBefore, _, _, _, _, _, _), Stack),
	member(class(ClassNameAfter, _, _, _, DayAfter, HourAfter, _), Stack),
	( DayBefore #= DayAfter #==> HourBefore #< HourAfter ) #/\ ( DayBefore #\= DayAfter #==> DayBefore #< DayAfter ),
	StackOut = [class(ClassNameBefore, _, _, _, DayBefore, HourBefore, _) | Stack].
% Both classes are already in stack, just adding constraintes
addClassToStack(Stack, classBefore([ClassNameBefore], [ClassNameAfter]), Stack) :-
	member(class(ClassNameBefore, _, _, _, DayBefore, HourBefore, _), Stack),
	member(class(ClassNameAfter, _, _, _, DayAfter, HourAfter, _), Stack),
	( DayBefore #= DayAfter #==> HourBefore #< HourAfter ) #/\ ( DayBefore #\= DayAfter #==> DayBefore #< DayAfter ).

% Constraintes on student number
addClassToStack(Stack, classStudents([ClassName], Number), _) :-
	member(class(ClassName, _, _, _, _, _, StudentsNumber), Stack),
	StudentsNumber #= Number.
addClassToStack(Stack, classStudents([ClassName], StudentsNumber), StackOut) :-
	\+ member(class(ClassName, _, _, _, _, _, _), Stack),
	StackOut = [class(ClassName, _, _, _, _, _, StudentsNumber) | Stack].

% Constraintes on class room
addClassToStack(Stack, classRoom([ClassName], RoomNumber), StackOut) :-
	\+ member(class(ClassName, _, _, _, _, _, _), Stack),
	Room #= RoomNumber,
	StackOut = [class(ClassName, _, _, Room, _, _, _) | Stack].
addClassToStack(Stack, classRoom([ClassName], RoomNumber), _) :-
	member(class(ClassName, _, _, Room, _, _, _), Stack),
	Room #= RoomNumber.

% Constraintes on same room classes
addClassToStack(Stack, sameRoom(ClassList), StackOut) :-
	goThroughSameRoom(ClassList, NewClasses, [Room1, Room2], Stack),
	Room1 #= Room2,
	append(NewClasses, Stack, StackOut).
addClassToStack(Stack, sameRoom(ClassList), StackOut) :-
	goThroughSameRoom(ClassList, NewClasses, [Room1, Room2, Room3], Stack),
	Room1 #= Room2,
	Room2 #= Room3,
	append(NewClasses, Stack, StackOut).

% Constraintes on same day classes
addClassToStack(Stack, sameDay(ClassList), StackOut) :-
	goThroughSameDay(ClassList, NewClasses, [Day1, Day2], Stack),
	Day1 #= Day2,
	append(NewClasses, Stack, StackOut).
addClassToStack(Stack, sameDay(ClassList), StackOut) :-
	goThroughSameDay(ClassList, NewClasses, [Day1, Day2, Day3], Stack),
	Day1 #= Day2,
	Day2 #= Day3,
	append(NewClasses, Stack, StackOut).

% Constraintes on same teacher classes
addClassToStack(Stack, sameTeacher(ClassList), StackOut) :-
	goThroughSameTeacher(ClassList, NewClasses, [Teacher1, Teacher2], Stack),
	Teacher1 #= Teacher2,
	append(NewClasses, Stack, StackOut).
addClassToStack(Stack, sameTeacher(ClassList), StackOut) :-
	goThroughSameTeacher(ClassList, NewClasses, [Teacher1, Teacher2, Teacher3], Stack),
	Teacher1 #= Teacher2,
	Teacher2 #= Teacher3,
	append(NewClasses, Stack, StackOut).

% Used to go through a list of Class and add to an accumulateur. This allows me to handle in a better way having several classes
% and prevent me to create 9 differents predicates, does c1 exists in stack but c2 and c3 not, etc, etc...
goThroughSameRoom([], [], [], _).
goThroughSameRoom([ClassName|RestClasses], [NewClass|ClassesOut], [NewRoom|RoomOut], Stack) :-
	\+ member(class(ClassName, _, _, _, _, _, _), Stack),
	NewClass = class(ClassName, _, _, Room, _, _, _),
	NewRoom = Room,
	goThroughSameRoom(RestClasses, ClassesOut, RoomOut, Stack).
goThroughSameRoom([ClassName|RestClasses], ClassesOut, [Room|RoomOut], Stack) :-
	member(class(ClassName, _, _, Room, _, _, _), Stack),
	%NewRoom = Room,
	goThroughSameRoom(RestClasses, ClassesOut, RoomOut, Stack).

goThroughSameDay([], [], [], _).
goThroughSameDay([ClassName|RestClasses], [NewClass|ClassesOut], [NewDay|RoomOut], Stack) :-
	\+ member(class(ClassName, _, _, _, _, _, _), Stack),
	NewClass = class(ClassName, _, _, _, Day, _, _),
	NewDay = Day,
	goThroughSameDay(RestClasses, ClassesOut, RoomOut, Stack).
goThroughSameDay([ClassName|RestClasses], ClassesOut, [NewDay|RoomOut], Stack) :-
	member(class(ClassName, _, _, _, Day, _, _), Stack),
	NewDay = Day,
	goThroughSameDay(RestClasses, ClassesOut, RoomOut, Stack).

goThroughSameTeacher([], [], [], _).
goThroughSameTeacher([ClassName|RestClasses], [NewClasses|ClassesOut], [Teachby|TeacherOut], Stack) :-
	\+ member(class(ClassName, _, _, _, _, _, _), Stack),
	NewClasses = class(ClassName, _, Teachby, _, _, _, _),
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
updateStack([Teacher|_], _, _) :- 
	\+ Teacher = teacher(_,_,_),
	throw("Error : Previous sentence should be teacher when he/she is used !").
updateStack([teacher(_, _, ClassList)|_], NewClass, _) :- 
	member(NewClass, ClassList),
	throw("Error : he/she teacher new class is already in stack !").
updateStack([teacher(TeacherName, TeacherNumber, ClassList)|Stack], NewClass, [teacher(TeacherName, TeacherNumber, NewClassList)|Stack]) :- 
	\+ member(NewClass, ClassList),
	append(ClassList, [NewClass], NewClassList).


% --
% -- FINAL CONSTRAINTES
% --

%% TimeTable(Data, TimeTable)
%
% TimeTable/2 Receives a list of sentence and creates representation send to TimeTable
%
% @param Data A list of sentences that should be parsed by the application.
% @param TimeTable The Data we call Stack in our application, filled with the goal representation used for all the constraintes, will be used to print the TimeTable.
%
timeTable(Data, TimeTable) :-
	parseText(Data, TimeTable),

	% Teachers
	length(TeacherNumber, 4),
	all_different(TeacherNumber),
	buildTeacherList(TimeTable, TeachersList),
	teacherConstraintes(TeacherNumber, TeachersList, TimeTable),

	% Rooms
	length(RoomNumber, 3),
	all_different(RoomNumber),
	buildRoomList(TimeTable, RoomsList),
	roomConstraintes(RoomNumber, RoomsList, TimeTable),

	% Classes
	length(ClassNumber, 5),
	all_different(ClassNumber),
	buildClassList(TimeTable, ClassesList),
	classConstraintes(ClassNumber, ClassesList, TimeTable, Variables),
	labeling([ffc], Variables).

%% buildTeacherList(Stack, ListTeacher)
%
% buildTeacherList/2 Takes the Stack and return a list with only the Teachers. Make it easier to creates the constraintes later
%
% @param Stack The Stack with all the representations
% @param ListTeacher The returned Teacher list
%
buildTeacherList([], []).
buildTeacherList([Teacher|Stack], [teacher(TeacherName, TeacherNumber, ClassList)|TeachersList]) :-
	Teacher = teacher(TeacherName, TeacherNumber, ClassList),
	buildTeacherList(Stack, TeachersList).
buildTeacherList([Elem|Stack], TeacherList) :-
	\+ Elem = teacher(_,_,_), 
	buildTeacherList(Stack, TeacherList). 

%% teacherConstraintes(Teachers, TeachersList, Stack)
%
% teacherConstraintes/3 Creates all the last constraintes on teachers and classes. For instance ensure that the Variable teachBy in Classes is equal to the teacher that teaches the class.
%
% @param Teachers A list of teacher number, used to ensure the number of teacher
% @param TeachersList The list of teacher
% @param Stack The Stack in order to check if classes already exist in the stack
%
teacherConstraintes([],[], _).
teacherConstraintes([Teacher|Teachers], [teacher(_, Teacher, ClassList)|TeachersList], Stack) :-
	teacher(Teacher),
	goThroughTeachBy(ClassList, Teacher, Stack),
	teacherConstraintes(Teachers, TeachersList, Stack).

%% goThroughTeachBy(ClassList, TeacherNumber, Stack)
%
% goThroughTeachBy/3 A teacher can teaches several classes, the function will go through these classes and add constraintes
%
% @param ClassList The classes the teacher teaches
% @param TeacherNumber The associated number to the teacher used for the Class constraintes
% @param Stack The usual Stack used to check if a class exist in the stack
%
goThroughTeachBy([], _, _).
goThroughTeachBy([Class|ClassList], TeacherNumber, Stack) :-
	member(class(Class, _,Teacher,_,_,_,_), Stack),
	Teacher #= TeacherNumber,
	goThroughTeachBy(ClassList, TeacherNumber, Stack).
goThroughTeachBy([Class|ClassList], TeacherNumber, Stack) :-
	\+ member(class(Class, _,_,_,_,_,_), Stack),
	goThroughTeachBy(ClassList, TeacherNumber, Stack).

%% buildRoomList(Stack, RoomsList)
%
% buildRoomList/2 Takes the Stack and return a list with only the Rooms. Make it easier to creates the constraintes later
%
% @param Stack The Stack with all the representations
% @param RoomsList The returned Room list
%
buildRoomList([], []).
buildRoomList([Room|Rooms], [Room|RoomsList]) :-
	Room = room(_, _, _),
	buildRoomList(Rooms, RoomsList).
buildRoomList([Room|Rooms], RoomsList) :-
	\+ Room = room(_,_,_), 
	buildRoomList(Rooms, RoomsList).

%% roomConstraintes(RoomNumber, RoomsList, Stack)
%
% roomConstraintes/3 Creates all the last constraintes on rooms. Associate a Room name with a room number, used later for the number of student in a class and only allowing classes size smaller than the room size.
%
% @param RoomNumber A list of room number
% @param RoomsList The list of Rooms
% @param Stack The Stack
%
roomConstraintes([], [], _).
roomConstraintes([RoomNumber|RoomsNumbers], [room(_, RoomNumber, Students)|RoomsList], TimeTable) :-
	room(RoomNumber, Students),
	roomConstraintes(RoomsNumbers, RoomsList, TimeTable).

%% buildClassList(Stack, ClassesList)
%
% buildClassList/2 Takes the Stack and return a list with only the Classes. Make it easier to creates the constraintes later
%
% @param Stack The Stack with all the representations
% @param ClassesList The returned Class list
%
buildClassList([], []).
buildClassList([Class|Stack], [Class|ClassesList]) :-
	Class = class(_, _, _, _, _, _, _),
	buildClassList(Stack, ClassesList).
buildClassList([Elem|Stack], ClassesList) :-
	\+ Elem = class(_,_,_,_,_,_,_), 
	buildClassList(Stack, ClassesList). 

%% classConstraintes(ClassNumber, ClassesList, Stack)
%
% classConstraintes/3 Creates all the last constraintes on classes. 
% Ensure that Day and Hour is within the domain 1..5. 5 days a week, 5 hours slot for a day.
% Makes the connexion with the room size and ensures the number of students for the class is smaller than the size room.
%
% @param ClassNumber A list of room number
% @param ClassesList The list of Classes
% @param Stack The Stack
%
classConstraintes([], [], _, []).
classConstraintes([ClassNumber|ClassNumbers], [class(ClassName, ClassNumber, Teachby, Room, Day, Hour, NumberStudents)|ClassesList], TimeTable, [Day, Hour|Variables]) :-
	class(ClassNumber, NumberStudents),
	Day in 1..5,
	Hour in 1..5,
	scheduleClassComparison(class(ClassName, ClassNumber, Teachby, Room, Day, Hour, NumberStudents), ClassesList),
	member(room(_,Room, SizeRoom), TimeTable),
	NumberStudents #=< SizeRoom,
	classConstraintes(ClassNumbers, ClassesList, TimeTable, Variables).

%% scheduleClassComparison(Class, ClassList)
%
% scheduleClassComparison/2 Compare one given class to the other classes in order to prevent classes in the same room at the same moment
% Also ensure that a teacher is not teaching two different classes at the same time
%
% @param Class The Class that is going to be compare to the other classes in the list
% @param ClassList The Class list that is going to be compared with
%
scheduleClassComparison(_, []).
scheduleClassComparison(class(_, _, Teach1, Room1, Day1, Hour1, _), [class(_, _, Teach2, Room2, Day2, Hour2, _)|Tail]):-
	%( Day1 #= Day2 #==> Hour1 #= Hour2 #==> Room1 #\= Room2),
	%((Day1 #= Day2 #/\ Hour1 #= Hour2) #==> Room1 #\= Room2),
	(Room1 #= Room2) #==> (Hour1 #\= Hour2),
	(Room1 #= Room2) #==> (Day1 #\= Day2),
	( Day1 #= Day2 #==> Hour1 #= Hour2 #==> Teach1 #\= Teach2),
	scheduleClassComparison(class(_, _, _, _, Day1, Hour1, _), Tail).

% --
% -- PARSING 
% --

%% splitFullstop(In, Out)
%
% splitFullstop/2 Receives a List of sentences and created a list of list of these sentences
%
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
%
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
	printClasses(SortedMondays, TimeTable),
	writeln("Tuesday : "),
	printClasses(SortedTuesdays, TimeTable),
	writeln("Wednesday : "),
	printClasses(SortedWednesdays, TimeTable),
	writeln("Thursday : "),
	printClasses(SortedThursdays, TimeTable),
	writeln("Friday : "),
	printClasses(SortedFridays, TimeTable),


	writeln("\n----- END OF TIMETABLE -----\n").

printRooms([]).
printRooms([room(RoomName, _, RoomSize)|TimeTable]) :-
	write("Room "),
	write_term(RoomName, []),
	write(" with "),
	write_term(RoomSize, []),
	write(" seats"),
  	writeln(""),
	printRooms(TimeTable).

printTeachers([]).
printTeachers([teacher(TeacherName, _, ClassList)|TimeTable]) :-
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

printClasses([], _).
printClasses([class(ClassName, _, Teachby, Room, Day, Hour, NumberStudents)|TimeTable], Stack) :-
	write("Class "),
	write_term(ClassName, []),
	write(" teaches by "), % name of teacher
	member(teacher(NameTeacher, Teachby,_), Stack),
	write_term(NameTeacher, []),
	write(" in room "),
	member(room(NameRoom, Room, _), Stack),
	write_term(NameRoom, []),
	printDay(Day, Hour),
	write(" with "),
	write_term(NumberStudents,[]),
	write(" students"),
	writeln(""),
	printClasses(TimeTable, Stack).

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

test([prof, steve, teaches, classes, c1, fullstop,
 prof, rob, teaches, classes, c2, fullstop,
 prof, junior, teaches, classes, c3, fullstop,
 he, also, teaches, class, c4, fullstop,
 prof, bobby, teaches, class, c5, fullstop,
 class, c1, has, 30, students, fullstop,
 class, c2, has, 35, students, fullstop,
 class, c3, has, 50, students, fullstop,
 class, c1, is, before, class, c3, fullstop,
 class, c5, is, before, class, c4, fullstop,
 classes, c1, and, c4, are, on, the, same, day, fullstop,
 classes, c2, and, c5, are, in, the, same, room, fullstop,
 %classes, c1, and, c2, have, the, same, teacher, fullstop,
 room, 102, seats, 100, students, fullstop,
 room, 202, seats, 35, students, fullstop,
 room, 303, seats, 60, students, fullstop ]).

%bug for same room, add doublons

%bug for same teacher, return false
%Teacher teaches class, should add the class in not found
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
	phrase(teacherSentence(Stack,[teacher(rob, _, [c1])]), [prof, rob, teaches, class, c1]),	
	phrase(teacherSentence(Stack,[teacher(steve, _, [c1,c2])]), [prof, steve, teaches, classes, c1, and, c2]),
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
	phrase(line(Stack,[teacher(steve, _, [c1,c2])]), [prof, steve, teaches, classes, c1, and, c2]),
	phrase(line(Stack, [class(c1, _,_,_,_,_,_), class(c2, _,_,_,_,_,_)]), [class, c1, is, before, class, c2]),
	phrase(line(Stack, [room(102, 3, 100)]), [room, 102, seats, 100, students]).


% --
% -- ANALYSE & COMMENTS
% --



