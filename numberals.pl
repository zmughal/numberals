
% number <-> name table {{{
number_to_name_t(0, zero).
number_to_name_t(1, one).
number_to_name_t(2, two).
number_to_name_t(3, three).
number_to_name_t(4, four).
number_to_name_t(5, five).
number_to_name_t(6, six).
number_to_name_t(7, seven).
number_to_name_t(8, eight).
number_to_name_t(9, nine).
number_to_name_t(10, ten).
number_to_name_t(11, eleven).
number_to_name_t(12, twelve).
number_to_name_t(13, thirteen).
number_to_name_t(14, fourteen).
number_to_name_t(15, fifteen).
number_to_name_t(16, sixteen).
number_to_name_t(17, seventeen).
number_to_name_t(18, eighteen).
number_to_name_t(19, nineteen).

number_to_name_t(20, twenty).
number_to_name_t(30, thirty).
number_to_name_t(40, forty).
number_to_name_t(50, fifty).
number_to_name_t(60, sixty).
number_to_name_t(70, seventy).
number_to_name_t(80, eighty).
number_to_name_t(90, ninety).
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

number_to_name(Num, Name) :- number_to_name_t(Num, Name), !.

number_to_name(

% tens {{{
number_to_name(Num, Name) :-
	% Num >= 21, Num =< 99,
	number_chars(Num, [ Tens, Ones ]), Ones \= '0',
	number_chars( Tens_Full, [ Tens, '0' ]),
	number_chars( Ones_Full, [ Ones ]),
	number_to_name_t(Tens_Full, Tens_Name),
	number_to_name_t(Ones_Full, Ones_Name),
	atom_concat('-', Ones_Name, Ones_Name_Hypen),
	atom_concat(Tens_Name, Ones_Name_Hypen, Name), !.
% }}}
% hundreds {{{
number_to_name(Num, Name) :-
	% Num >= 100, Num =< 999,
        number_chars( Num, [ Hundreds, Tens, Ones] ),
	number_chars( Rest_Num, [ Tens,  Ones ]),
	number_chars( Hundreds_Num, [Hundreds]),
	number_to_name_t( Hundreds_Num, Hundreds_Name),
	hundred_build(Rest_Num, Rest_Name),
	atom_concat(Hundreds_Name, ' hundred', Hundreds_Name_Part),
	atom_concat(Hundreds_Name_Part, Rest_Name, Name), !.

hundred_build( 0, '') :- !.% {{{
hundred_build( Rest_Num, Rest_Name) :-
	Rest_Num \= 0,
	number_to_name(Rest_Num, Rest_Name_Part),
	atom_concat(' and ', Rest_Name_Part, Rest_Name), !.
% }}}

% }}}



% --> 

% vim:ft=prolog:fdm=marker
