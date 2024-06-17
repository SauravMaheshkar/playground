testcase_one_input_one(branch(branch(branch(nil, 1, nil), 4, branch(nil, 2, nil)), 3, branch(nil, 5, nil))).
testcase_one_input_two(branch(branch(nil, 1, nil), 4, branch(nil, 2, nil))).
testcase_one_output(true).

testcase_two_input_one(branch(branch(branch(nil, 1, nil), 4, branch(branch(nil, 0, nil), 2, nil)), 3, branch(nil, 5, nil))).
testcase_two_input_two(branch(branch(nil, 1, nil), 4, branch(nil, 2, nil))).
testcase_two_output(false).

check_same(nil, nil, true).
check_same(nil, _, false).
check_same(_, nil, false).
check_same(branch(Left1, X1, Right1), branch(Left2, X2, Right2), Result) :-
    check_same(Left1, Left2, LeftResult),
    check_same(Right1, Right2, RightResult),
    ( X1 == X2, LeftResult == true, RightResult == true -> Result=true; Result = false).

is_subtree(_, nil, true).
is_subtree(nil, _, false).
is_subtree(Root, SubTree, true) :-
    check_same(Root, SubTree, true).
is_subtree(branch(Left, _, _), SubTree, true) :-
    is_subtree(Left, SubTree, true).
is_subtree(branch(_, _, Right), SubTree, true) :-
    is_subtree(Right, SubTree, true).
is_subtree(_, _, false).

test_subtree(Input1, Input2, ExpectedOutput) :-
    is_subtree(Input1, Input2, Result),
    Result == ExpectedOutput.

run_tests :-
    testcase_one_input_one(Input11),
    testcase_one_input_two(Input12),
    testcase_one_output(ExpectedOutput1),
    ( test_subtree(Input11, Input12, ExpectedOutput1) ->
        true;
        writeln('Failed Test Case 1')
    ),

    testcase_two_input_one(Input21),
    testcase_two_input_two(Input22),
    testcase_two_output(ExpectedOutput2),
    ( test_subtree(Input21, Input22, ExpectedOutput2) ->
        true;
        writeln('Failed Test Case 2')
    ).
