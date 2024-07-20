testcase_one_input_one(branch(branch(nil, 2, nil), 1, branch(nil, 3, nil))).
testcase_one_input_two(branch(branch(nil, 2, nil), 1, branch(nil, 3, nil))).
testcase_one_output(true).

testcase_two_input_one(branch(branch(nil, 2, nil), 1, nil)).
testcase_two_input_two(branch(nil, 1, branch(nil, 2, nil))).
testcase_two_output(false).

testcase_three_input_one(branch(branch(nil, 2, nil), 1, branch(nil, 1, nil))).
testcase_three_input_two(branch(branch(nil, 1, nil), 1, branch(nil, 2, nil))).
testcase_three_output(false).

check_same(nil, nil, true).
check_same(nil, _, false).
check_same(_, nil, false).
check_same(branch(Left1, X1, Right1), branch(Left2, X2, Right2), Result) :-
    check_same(Left1, Left2, LeftResult),
    check_same(Right1, Right2, RightResult),
    ( X1 == X2, LeftResult == true, RightResult == true -> Result=true; Result = false).

test_same(Input1, Input2, ExpectedOutput) :-
    check_same(Input1, Input2, Result),
    Result == ExpectedOutput.

run_tests :-
    testcase_one_input_one(Input11),
    testcase_one_input_two(Input12),
    testcase_one_output(ExpectedOutput1),
    ( test_same(Input11, Input12, ExpectedOutput1) ->
        true;
        writeln('Failed Test Case 1')
    ),

    testcase_two_input_one(Input21),
    testcase_two_input_two(Input22),
    testcase_two_output(ExpectedOutput2),
    ( test_same(Input21, Input22, ExpectedOutput2) ->
        true;
        writeln('Failed Test Case 2')
    ),

    testcase_three_input_one(Input31),
    testcase_three_input_two(Input32),
    testcase_three_output(ExpectedOutput3),
    ( test_same(Input31, Input32, ExpectedOutput3) ->
        true;
        writeln('Failed Test Case 3')
    ).
