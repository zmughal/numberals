:- begin_tests(numberals).
:- consult('numberals.pl').

logging :- fail.

log_writef(A, B) :- logging -> writef(A, B); true.
log_writef(A) :- log_writef(A, []).

test_both_ways(Num, Name) :-
	ground([Num, Name]),
	log_writef( '%q -> %s : ', [Num,  Name] ), number_to_name(Num, Test_Name), log_writef( 'Got %s', [Test_Name]), log_writef('\n'), Name = Test_Name,
	log_writef( '%s -> %q : ', [Name,  Num] ), number_to_name(Test_Num, Name), log_writef( 'Got %q', [Test_Num ]), log_writef('\n'), log_writef('\n'), Num  = Test_Num.

test_name_fail(Name) :-
	log_writef( 'Testing name %s : ', [Name]),
	(\+ number_to_name(_, Name) -> log_writef('not found - ok'), log_writef('\n');
		log_writef('found - fail'), log_writef('\n')), log_writef('\n').
test_num_fail(Num) :-
	log_writef( 'Testing number %q : ', [Num]),
	(\+ number_to_name(Num, _) -> log_writef('not found - ok'), log_writef('\n');
		log_writef('found - fail'), log_writef('\n')), log_writef('\n').

test_both_ways_test([0], "zero").
test_both_ways_test(['-',0], "negative zero"). % signed zero
test_both_ways_test([1,0], "ten").
test_both_ways_test([-,1,0], "negative ten").
test_both_ways_test([1,4], "fourteen").
test_both_ways_test([-,1,4], "negative fourteen").
test_both_ways_test([2,0], "twenty").
test_both_ways_test([-,2,0], "negative twenty").
test_both_ways_test([2,1], "twenty-one").
test_both_ways_test([-,2,1], "negative twenty-one").
test_both_ways_test([3,2], "thirty-two").
test_both_ways_test([-,3,2], "negative thirty-two").
test_both_ways_test([3,5], "thirty-five").
test_both_ways_test([-,3,5], "negative thirty-five").
test_both_ways_test([1, 3,5], "one hundred and thirty-five").
test_both_ways_test([-,1, 3,5], "negative one hundred and thirty-five").
test_both_ways_test([1,0,0], "one hundred").
test_both_ways_test([-,1,0,0], "negative one hundred").
test_both_ways_test([2,0,0], "two hundred").
test_both_ways_test([-,2,0,0], "negative two hundred").
test_both_ways_test([2,0,1], "two hundred and one").
test_both_ways_test([-,2,0,1], "negative two hundred and one").
test_both_ways_test([2,2,0], "two hundred and twenty").
test_both_ways_test([-,2,2,0], "negative two hundred and twenty").
test_both_ways_test([2,2,5], "two hundred and twenty-five").
test_both_ways_test([-,2,2,5], "negative two hundred and twenty-five").
test_both_ways_test([3,1,0], "three hundred and ten").
test_both_ways_test([-,3,1,0], "negative three hundred and ten").
test_both_ways_test([9,9,9], "nine hundred and ninety-nine").
test_both_ways_test([-,9,9,9], "negative nine hundred and ninety-nine").

test(test_both_ways, [nondet, forall( test_both_ways_test(Num, Name) )]) :-
	test_both_ways(Num, Name).

test_name_fail_test("zero hundred").
test(test_name_fail, [nondet, forall( test_name_fail_test(Name) )]) :-
	test_name_fail(Name).

test_num_fail_test([1,-,0]).
test_num_fail_test([-,-,0]).
test_num_fail_test([-,-,-]).
test_num_fail_test([0,0,0]).
test(test_num_fail, [nondet, forall( test_num_fail_test(Num) )]) :-
	test_num_fail(Num).

:- end_tests(numberals).

% vim:ft=prolog:fdm=marker
