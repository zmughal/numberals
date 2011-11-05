% Numberals

isdigit(X) :- member(X, [0,1,2,3,4,5,6,7,8,9]).

% number <-> name table {{{
number_to_name_t([0], zero).
number_to_name_t([1], one).
number_to_name_t([2], two).
number_to_name_t([3], three).
number_to_name_t([4], four).
number_to_name_t([5], five).
number_to_name_t([6], six).
number_to_name_t([7], seven).
number_to_name_t([8], eight).
number_to_name_t([9], nine).
number_to_name_t([1,0], ten).
number_to_name_t([1,1], eleven).
number_to_name_t([1,2], twelve).
number_to_name_t([1,3], thirteen).
number_to_name_t([1,4], fourteen).
number_to_name_t([1,5], fifteen).
number_to_name_t([1,6], sixteen).
number_to_name_t([1,7], seventeen).
number_to_name_t([1,8], eighteen).
number_to_name_t([1,9], nineteen).

number_to_name_t([2,0], twenty).
number_to_name_t([3,0], thirty).
number_to_name_t([4,0], forty).
number_to_name_t([5,0], fifty).
number_to_name_t([6,0], sixty).
number_to_name_t([7,0], seventy).
number_to_name_t([8,0], eighty).
number_to_name_t([9,0], ninety).
% }}}
% powers table {{{
power_name(3, thousand).
power_name(6, million).
power_name(9, billion).
power_name(12, trillion).
power_name(15, quadrillion).
power_name(18, quintillion).
power_name(21, sextillion).
power_name(24, septillion).
power_name(27, octillion).
power_name(30, nonillion).
power_name(33, decillion).
power_name(36, undecillion).
power_name(39, duodecillion).
power_name(42, tredecillion).
power_name(45, quattuordecillion).
power_name(48, quindecillion).
% }}}

number_to_name(Num, Name) :- number_to_name_t(Num, Name).

% negative {{{
number_to_name([-|Positive], Name) :-
	ground(Positive),
	Positive \= [-|_],
	number_to_name(Positive, Positive_Name),
	string_concat('negative ', Positive_Name, Name_Str),
	string_to_atom(Name_Str, Name).
number_to_name(Number, Name) :-
	ground(Name),
	string_concat('negative ', Positive_Name_Str, Name),
	string_to_atom(Positive_Name_Str, Positive_Name),
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
	string_concat('-', Ones_Name, Ones_Name_Hypen),
	string_concat(Tens_Name, Ones_Name_Hypen, Name_Str),
	string_to_atom(Name_Str, Name).
% name -> number
number_to_name([Tens, Ones], Name) :-
	ground(Name),
	string_concat(Tens_Name_Str, Ones_Name_Hypen, Name),
	string_concat('-', Ones_Name_Str, Ones_Name_Hypen),
	string_to_atom(Tens_Name_Str, Tens_Name),
	string_to_atom(Ones_Name_Str, Ones_Name),
	number_to_name_t([Tens, 0], Tens_Name),
	number_to_name_t([ Ones ], Ones_Name).
% }}}
% hundreds {{{
% Num >= 100, Num =< 999,
% number -> name
number_to_name([Hundreds, Tens, Ones ], Name) :-
	ground([Hundreds, Tens, Ones]),
	Hundreds > 0,
	number_to_name_t( [Hundreds], Hundreds_Name),
	hundred_build([Tens, Ones], Rest_Name),
	string_concat(Hundreds_Name, ' hundred', Hundreds_Name_Part),
	string_concat(Hundreds_Name_Part, Rest_Name, Name_Str),
	string_to_atom(Name_Str, Name).
% name -> number
number_to_name([Hundreds, Tens, Ones ], Name) :-
	ground(Name),
	string_concat(Hundreds_Name_Part, Rest_Name_Str, Name),
	string_concat(Hundreds_Name_Str, ' hundred', Hundreds_Name_Part),
	string_to_atom(Hundreds_Name_Str, Hundreds_Name),
	string_to_atom(Rest_Name_Str, Rest_Name),
	number_to_name_t( [Hundreds], Hundreds_Name),
	Hundreds > 0,
	hundred_build([Tens, Ones], Rest_Name).
% }}}

% hundred helpers {{{
number_to_name_prefix([0, Tens, Ones], Name) :- number_to_name( [Tens, Ones], Name).
number_to_name_prefix([Hundreds, Tens, Ones], Name) :- number_to_name( [Hundreds, Tens, Ones], Name).
number_to_name_prefix([0, Ones], Name) :- number_to_name( [Ones], Name).
number_to_name_prefix([Tens, Ones], Name) :- number_to_name( [Tens, Ones], Name).

% hundred build {{{

hundred_build( [Tens, Ones], Rest_Name) :-
	ground([Tens, Ones]),
	[Tens, Ones] \= [ 0, 0],
	number_to_name_prefix([Tens, Ones], Rest_Name_Part),
	string_concat(' and ', Rest_Name_Part, Rest_Name_Str),
	string_to_atom(Rest_Name_Str, Rest_Name).
hundred_build( [Tens, Ones], Rest_Name) :-
	ground(Rest_Name),
	string_concat(' and ', Rest_Name_Part_Str, Rest_Name),
	string_to_atom(Rest_Name_Part_Str, Rest_Name_Part),
	number_to_name_prefix([Tens, Ones], Rest_Name_Part).
hundred_build( [0, 0], '').
% }}}

% }}}

test :-
	test_both_ways([0], 'zero'),
	test_both_ways(['-',0], 'negative zero'), % signed zero
	test_both_ways([1,0], 'ten'),
	test_both_ways([-,1,0], 'negative ten'),
	test_both_ways([1,4], 'fourteen'),
	test_both_ways([-,1,4], 'negative fourteen'),
	test_both_ways([2,0], 'twenty'),
	test_both_ways([-,2,0], 'negative twenty'),
	test_both_ways([2,1], 'twenty-one'),
	test_both_ways([-,2,1], 'negative twenty-one'),
	test_both_ways([3,2], 'thirty-two'),
	test_both_ways([-,3,2], 'negative thirty-two'),
	test_both_ways([3,5], 'thirty-five'),
	test_both_ways([-,3,5], 'negative thirty-five'),
	test_both_ways([1, 3,5], 'one hundred and thirty-five'),
	test_both_ways([-,1, 3,5], 'negative one hundred and thirty-five'),
	test_both_ways([1,0,0], 'one hundred'),
	test_both_ways([-,1,0,0], 'negative one hundred'),
	test_both_ways([2,0,0], 'two hundred'),
	test_both_ways([-,2,0,0], 'negative two hundred'),
	test_both_ways([2,0,1], 'two hundred and one'),
	test_both_ways([-,2,0,1], 'negative two hundred and one'),
	test_both_ways([2,2,0], 'two hundred and twenty'),
	test_both_ways([-,2,2,0], 'negative two hundred and twenty'),
	test_both_ways([2,2,5], 'two hundred and twenty-five'),
	test_both_ways([-,2,2,5], 'negative two hundred and twenty-five'),
	test_both_ways([3,1,0], 'three hundred and ten'),
	test_both_ways([-,3,1,0], 'negative three hundred and ten'),
	test_both_ways([9,9,9], 'nine hundred and ninety-nine'),
	test_both_ways([-,9,9,9], 'negative nine hundred and ninety-nine').

test_both_ways(Num, Name) :-
	writef( '%q -> %q : ', [Num,  Name] ), number_to_name(Num, Test_Name), writef( 'Got %q', [Test_Name]), nl, Name = Test_Name,
	writef( '%q -> %q : ', [Name,  Num] ), number_to_name(Test_Num, Name), writef( 'Got %q', [Test_Num ]), nl, nl, Num  = Test_Num.

% --> 

% vim:ft=prolog:fdm=marker
