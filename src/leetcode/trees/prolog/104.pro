testcase_one_input(branch(branch(nil, 9, nil), 3, branch(branch(nil, 15, nil), 20, branch(nil, 7, nil)))).
testcase_one_output(3).

testcase_two_input(branch(branch(nil, 1, nil), nil, branch(nil, 2, nil))).
testcase_two_output(2).

maximum_depth(nil, 0).
maximum_depth(branch(Left, _, Right), Depth) :-
    maximum_depth(Left, LeftDepth),
    maximum_depth(Right, RightDepth),
    Depth is 1 + max(LeftDepth, RightDepth).

test_maximum_depth(Input, ExpectedOutput) :-
    maximum_depth(Input, Result),
    Result =:= ExpectedOutput.

run_tests :-
    testcase_one_input(Input1),
    testcase_one_output(ExpectedOutput1),
    ( test_maximum_depth(Input1, ExpectedOutput1) ->
        true;
        writeln('Failed Test Case 1')
    ),

    testcase_two_input(Input2),
    testcase_two_output(ExpectedOutput2),
    ( test_maximum_depth(Input2, ExpectedOutput2) ->
        true;
        writeln('Failed Test Case 2')
    ).
