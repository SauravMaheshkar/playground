testcase_one_input(branch(branch(nil, 9, nil), 3, branch(branch(nil, 15, nil), 20, branch(nil, 7, nil)))).
testcase_one_output(true).

testcase_two_input(branch(branch(branch(branch(nil, 4, nil), 3, branch(nil, 4, nil)), 2, branch(nil, 3, nil)), 1, branch(nil, 2, nil))).
testcase_two_output(false).

testcase_three_input(nil).
testcase_three_output(true).

check_balance(nil, 0).
check_balance(branch(Left, _, Right), Height) :-
    check_balance(Left, LeftHeight),
    check_balance(Right, RightHeight),
    ( LeftHeight =:= -1
    ; RightHeight =:= -1
    ; abs(LeftHeight - RightHeight) > 1
    -> Height is -1
    ; Height is max(LeftHeight, RightHeight) + 1
    ).

is_balanced(Tree, Result) :-
    check_balance(Tree, Height),
    ( Height =\= -1 -> Result = true ; Result = false ).

test_balanced(Input, ExpectedOutput) :-
    is_balanced(Input, Result),
    Result == ExpectedOutput.

run_tests :-
    testcase_one_input(Input1),
    testcase_one_output(ExpectedOutput1),
    ( test_balanced(Input1, ExpectedOutput1) ->
        true;
        writeln('Failed Test Case 1')
    ),

    testcase_two_input(Input2),
    testcase_two_output(ExpectedOutput2),
    ( test_balanced(Input2, ExpectedOutput2) ->
        true;
        writeln('Failed Test Case 2')
    ),

    testcase_three_input(Input3),
    testcase_three_output(ExpectedOutput3),
    ( test_balanced(Input3, ExpectedOutput3) ->
        true;
        writeln('Failed Test Case 3')
    ).
