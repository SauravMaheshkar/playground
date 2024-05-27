use crate::eval::{constructor, Tree, TreeBoolTestCase};
use crate::utils::parse_json_array;

fn check_balanced(root: Option<Tree>) -> i64 {
    match root {
        Some(node) => {
            let node_ref = node.borrow();
            let left_balance = check_balanced(node_ref.left.clone());
            let right_balance = check_balanced(node_ref.right.clone());

            if left_balance == -1 || right_balance == -1 || (left_balance - right_balance).abs() > 1
            {
                return -1;
            }

            left_balance.max(right_balance) + 1
        }
        None => 0,
    }
}

fn is_binary_tree_balanced(root: Option<Tree>) -> bool {
    match root {
        Some(node) => {
            if check_balanced(Some(node)) == -1 {
                false
            } else {
                true
            }
        }
        None => true,
    }
}

pub fn main(test_cases: Vec<TreeBoolTestCase>) {
    for test_case in test_cases {
        let input: Vec<Option<i64>> = parse_json_array::<i64>(&test_case.input);
        let expected_output: bool = test_case.output;

        let input_tree = constructor(&input);
        let output: bool = is_binary_tree_balanced(input_tree);

        assert_eq!(output, expected_output, "Test Case {:?} failed", input);
    }
    println!("Tests ran successfully for problem ID 110");
}
