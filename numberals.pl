% Numberals

%:- use_module(library(chr)).
%:- chr_type digit ---> 0; 1; 2; 3; 4; 5; 6; 7; 8; 9.
%:- chr_type positional_system ---> [digit]; [digit|list(digit)]; ['-', digit|list(digit)].
%:- chr_type list(T) ---> []; [T|list(T)].
%:- chr_constraint chr_number_to_name(?positional_system, ?list(int)).
%chr_number_to_name([0], "zero") ==> true.


isdigit(X) :- member(X, [0,1,2,3,4,5,6,7,8,9]).

% number <-> name table {{{
number_to_name_t([0], "zero").
number_to_name_t([1], "one").
number_to_name_t([2], "two").
number_to_name_t([3], "three").
number_to_name_t([4], "four").
number_to_name_t([5], "five").
number_to_name_t([6], "six").
number_to_name_t([7], "seven").
number_to_name_t([8], "eight").
number_to_name_t([9], "nine").
number_to_name_t([1,0], "ten").
number_to_name_t([1,1], "eleven").
number_to_name_t([1,2], "twelve").
number_to_name_t([1,3], "thirteen").
number_to_name_t([1,4], "fourteen").
number_to_name_t([1,5], "fifteen").
number_to_name_t([1,6], "sixteen").
number_to_name_t([1,7], "seventeen").
number_to_name_t([1,8], "eighteen").
number_to_name_t([1,9], "nineteen").

number_to_name_t([2,0], "twenty").
number_to_name_t([3,0], "thirty").
number_to_name_t([4,0], "forty").
number_to_name_t([5,0], "fifty").
number_to_name_t([6,0], "sixty").
number_to_name_t([7,0], "seventy").
number_to_name_t([8,0], "eighty").
number_to_name_t([9,0], "ninety").
% }}}
% powers table {{{
power_name(0, "").
power_name(3, "thousand").
power_name(6, "million").
power_name(9, "billion").
power_name(12, "trillion").
power_name(15, "quadrillion").
power_name(18, "quintillion").
power_name(21, "sextillion").
power_name(24, "septillion").
power_name(27, "octillion").
power_name(30, "nonillion").
power_name(33, "decillion").
power_name(36, "undecillion").
power_name(39, "duodecillion").
power_name(42, "tredecillion").
power_name(45, "quattuordecillion").
power_name(48, "quindecillion").
% }}}

number_to_name(Num, Name) :- number_to_name_t(Num, Name).

% negative {{{
number_to_name([-|Positive], Name) :-
	ground(Positive),
	Positive \= [-|_],
	number_to_name(Positive, Positive_Name),
	append(["negative ", Positive_Name], Name).
number_to_name([-|Positive], Name) :-
	ground(Name),
	append(["negative ", Positive_Name], Name),
	number_to_name(Positive, Positive_Name),
	Positive \= [-|_].
% }}}
% tens {{{
% Num >= 21, Num =< 99,
% number -> name
number_to_name([Tens, Ones], Name) :-
	ground([Tens, Ones]),
	isdigit(Tens), isdigit(Ones),
	number_to_name_t([Tens, 0], Tens_Name),
	number_to_name_t([ Ones ], Ones_Name),
	Tens \= 1, Ones \= 0,
	append([Tens_Name, "-", Ones_Name], Name).
% name -> number
number_to_name([Tens, Ones], Name) :-
	ground(Name),
	append([Tens_Name, "-", Ones_Name], Name),
	isdigit(Tens),
	number_to_name_t([Tens, 0], Tens_Name),
	isdigit(Ones),
	number_to_name_t([ Ones ], Ones_Name).
% }}}
% hundreds {{{
% Num >= 100, Num =< 999,
% number -> name
number_to_name([Hundreds, Tens, Ones ], Name) :-
	ground([Hundreds, Tens, Ones]),
	isdigit(Hundreds), isdigit(Tens), isdigit(Ones),
	Hundreds > 0,
	number_to_name_t( [Hundreds], Hundreds_Name),
	hundred_build([Tens, Ones], Rest_Name),
	append([Hundreds_Name, " hundred", Rest_Name], Name).
% name -> number
number_to_name([Hundreds, Tens, Ones ], Name) :-
	ground(Name),
	append([Hundreds_Name, " hundred", Rest_Name], Name),
	number_to_name_t( [Hundreds], Hundreds_Name),
	isdigit(Hundreds), Hundreds > 0,
	isdigit(Tens), isdigit(Ones),
	hundred_build([Tens, Ones], Rest_Name).
% }}}
% group {{{
number_to_name(Num, Name) :-
	ground(Num), length(Num, NumL), NumL > 3,
	append(First, NumNext, Num),
	length(First, First_l), First_l < 4,
	length(NumNext, NumNext_l), NumNext_l mod 3 =:= 0,
	number_to_name_add_prefix(First, FirstPrefix),
	FirstPrefix \= [0, 0, 0],
	append(FirstPrefix, NumNext, PrefixNum),
	number_to_name_group(PrefixNum, Name, NumNext_l).
number_to_name(Num, Name) :-
	ground(Name),
	% a non-empty group name must be in Name
	append([PrefixName," ", Group_Name, _], Name), power_name(Group, Group_Name), Group > 0,
	number_to_name_prefix(FirstPrefix, PrefixName),
	FirstPrefix \= [0, 0, 0],
	PrefixNumL is Group + 3, length(PrefixNum, PrefixNumL),
	number_to_name_group(PrefixNum, Name, Group),
	% remove prefix: [0,0,5] -> [5]
	append(FirstPrefix, NumNext, PrefixNum),
	number_to_name_add_prefix(FirstPrefixNoZero, FirstPrefix), FirstPrefixNoZero \= [0|_],
	append(FirstPrefixNoZero, NumNext, Num).
number_to_name_group([H,T,O|NumNext], Name, Group) :-
	ground(Group), Group >= 0, ground([H,T,O|NumNext]),
	( [H, T, O] = [0, 0, 0] -> Group_Name = ""; power_name(Group, Group_Name) ),
	number_to_name_prefix([H,T,O], Group_Prefix),
	GroupNext is Group - 3,
	number_to_name_group(NumNext, NameNext, GroupNext),
	(Group_Name == "" -> Space = ""; Space = " "),
	add_space_nextname(NameNext, NameNextS),
	append([Group_Prefix, Space, Group_Name, NameNextS], Name).
number_to_name_group([H,T,O|NumNext], Name, Group) :-
	ground(Name), Group > 0, GroupNext is Group - 3,
	(
		(power_name(Group, Group_Name), (Group_Name == "" -> Space = ""; Space = " "),
			append([Group_Prefix, Space, Group_Name, NameNextS], Name),
			Group_Prefix \= "zero",
			number_to_name_prefix([H,T,O], Group_Prefix));
		(Group_Prefix = "", Group_Name = "",
			append([Group_Prefix, Group_Name, NameNextS], Name),
			number_to_name_prefix([H,T,O], Group_Prefix))
	),
	remove_space(NameNextS, NameNext),
	number_to_name_group(NumNext, NameNext, GroupNext).
number_to_name_group([H,T,O], Name, 0) :- number_to_name_prefix([H,T,O], Name).
number_to_name_group([], "", _).

add_space_nextname("", ""). % empty string
add_space_nextname([32|NextName], [32|NextName]). % already has a space
add_space_nextname([First|NextName], [32,First|NextName]) :- First \= 32. % add space

remove_space("", "").
remove_space([32,First|NextName], [First|NextName]) :- First \= 32.
remove_space([First|NextName], [First|NextName]) :- [First|NextName] \= "", First \= 32.
% }}}
% hundred helpers {{{
number_to_name_prefix([       0,    0,    0], "") :- !.	% cut to prevent other unifications below
number_to_name_prefix([       0,    0, Ones], Name) :- ground(Ones), Ones \= 0, number_to_name( [Ones], Name).
number_to_name_prefix([       0, Tens, Ones], Name) :- ground(Tens), Tens \= 0, number_to_name( [Tens, Ones], Name).
number_to_name_prefix([Hundreds, Tens, Ones], Name) :- ground([Hundreds, Tens, Ones]), [Hundreds, Tens, Ones] \= [0, 0, 0], number_to_name( [Hundreds, Tens, Ones], Name).
number_to_name_prefix([   0, Ones], Name) :- ground(Ones), number_to_name( [Ones], Name).
number_to_name_prefix([Tens, Ones], Name) :- ground([Tens, Ones]), Tens \= 0, number_to_name( [Tens, Ones], Name).
number_to_name_prefix([One], Name) :- ground([One]), number_to_name([One], Name).

number_to_name_prefix([Hundreds, Tens, Ones], Name) :- ground(Name),
	\+ (append([_, GroupName, _],Name), GroupName \= "", power_name(_, GroupName)),
	member(NumLen, [1,2,3]), length(Num, NumLen), number_to_name( Num, Name),
	number_to_name_add_prefix(Num, [Hundreds, Tens, Ones]).

hundred_build( [Tens, Ones], Rest_Name) :-
	ground([Tens, Ones]),
	[Tens, Ones] \= [ 0, 0],
	number_to_name_prefix([Tens, Ones], Rest_Name_Part),
	append([" and ", Rest_Name_Part], Rest_Name).
hundred_build( [Tens, Ones], Rest_Name) :-
	ground(Rest_Name),
	append([" and ", Rest_Name_Part], Rest_Name),
	isdigit(Tens), isdigit(Ones),
	number_to_name_prefix([Tens, Ones], Rest_Name_Part),
	[Tens, Ones] \= [ 0, 0].
hundred_build( [0, 0], "").
% }}}


number_to_name_add_prefix([H,T,O], [H,T,O]).
number_to_name_add_prefix([  T,O], [0,T,O]).
number_to_name_add_prefix([    O], [0,0,O]).


% vim:ft=prolog:fdm=marker
