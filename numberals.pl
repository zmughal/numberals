% Numberals

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
number_to_name(Number, Name) :-
	ground(Name),
	append(["negative ", Positive_Name], Name),
	number_to_name(Positive, Positive_Name),
	Positive \= [-|_],
	Number = [-|Positive].

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
	number_to_name_t([Tens, 0], Tens_Name),
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
	Hundreds > 0,
	hundred_build([Tens, Ones], Rest_Name).
% }}}

% hundred helpers {{{
number_to_name_prefix([       0,    0,    0], "").
number_to_name_prefix([       0, Tens, Ones], Name) :- number_to_name( [Tens, Ones], Name).
number_to_name_prefix([Hundreds, Tens, Ones], Name) :- number_to_name( [Hundreds, Tens, Ones], Name).
number_to_name_prefix([   0, Ones], Name) :- number_to_name( [Ones], Name).
number_to_name_prefix([Tens, Ones], Name) :- number_to_name( [Tens, Ones], Name).
number_to_name_prefix([One], Name) :- number_to_name([One], Name).

hundred_build( [Tens, Ones], Rest_Name) :-
	ground([Tens, Ones]),
	[Tens, Ones] \= [ 0, 0],
	number_to_name_prefix([Tens, Ones], Rest_Name_Part),
	append([" and ", Rest_Name_Part], Rest_Name).
hundred_build( [Tens, Ones], Rest_Name) :-
	ground(Rest_Name),
	append([" and ", Rest_Name_Part], Rest_Name),
	number_to_name_prefix([Tens, Ones], Rest_Name_Part),
	[Tens, Ones] \= [ 0, 0].
hundred_build( [0, 0], "").
% }}}

% }}}

% --> 

% vim:ft=prolog:fdm=marker
