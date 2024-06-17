testcase_one_input(branch(branch(branch(nil, 4, nil), 2, branch(nil, 5, nil)), 1, branch(nil, 3, nil))).
testcase_one_output(3).

testcase_two_input(branch(branch(nil, 1, nil), 2, nil)).
testcase_two_output(1).

maximum_depth(nil, 0).
maximum_depth(branch(Left, _, Right), Depth) :-
    maximum_depth(Left, LeftDepth),
    maximum_depth(Right, RightDepth),
    Depth is 1 + max(LeftDepth, RightDepth).

diameter(nil, 0).
diameter(branch(Left, _, Right), Diameter) :-
    maximum_depth(Left, LeftDepth),
    maximum_depth(Right, RightDepth),
    RootDiameter is LeftDepth + RightDepth,

    diameter(Left, LeftDiameter),
    diameter(Right, RightDiameter),
    Diameter is max(RootDiameter, max(LeftDiameter, RightDiameter)).

test_diameter(Input, ExpectedOutput) :-
    diameter(Input, Result),
    Result =:= ExpectedOutput.

run_tests :-
    testcase_one_input(Input1),
    testcase_one_output(ExpectedOutput1),
    ( test_diameter(Input1, ExpectedOutput1) ->
        true;
        writeln('Failed Test Case 1')
    ),

    testcase_two_input(Input2),
    testcase_two_output(ExpectedOutput2),
    ( test_diameter(Input2, ExpectedOutput2) ->
        true;
        writeln('Failed Test Case 2')
    ).
