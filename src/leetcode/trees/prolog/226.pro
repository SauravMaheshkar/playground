testcase_one_input(branch(branch(branch(nil, 1, nil), 2, branch(nil, 3, nil)), 4, branch(branch(nil, 6, nil), 7, branch(nil, 9, nil)))).
testcase_one_output(branch(branch(branch(nil, 9, nil), 7, branch(nil, 6, nil)), 4, branch(branch(nil, 3, nil), 2, branch(nil, 1, nil)))).

testcase_two_input(branch(branch(nil, 1, nil), 2, branch(nil, 3, nil))).
testcase_two_output(branch(branch(nil, 3, nil), 2, branch(nil, 1, nil))).

testcase_three_input(nil).
testcase_three_output(nil).

invert(nil, nil).
invert(branch(Left, Value, Right), branch(InvertedRight, Value, InvertedLeft)) :-
    invert(Left, InvertedLeft),
    invert(Right, InvertedRight).

test_invert(Input, ExpectedOutput) :-
    invert(Input, Result),
    Result == ExpectedOutput.

run_tests :-
    testcase_one_input(Input1),
    testcase_one_output(ExpectedOutput1),
    ( test_invert(Input1, ExpectedOutput1) ->
        true;
        writeln('Failed Test Case 1')
    ),

    testcase_two_input(Input2),
    testcase_two_output(ExpectedOutput2),
    ( test_invert(Input2, ExpectedOutput2) ->
        true;
        writeln('Failed Test Case 2')
    ),

    testcase_three_input(Input3),
    testcase_three_output(ExpectedOutput3),
    ( test_invert(Input3, ExpectedOutput3) ->
        true;
        writeln('Failed Test Case 3')
    ).
