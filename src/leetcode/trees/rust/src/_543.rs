use crate::eval::{constructor, Tree, TreeNumTestCase};
use crate::utils::parse_json_array;

fn recursive_max_depth_binary_tree(root: Option<Tree>) -> i64 {
    root.iter()
        .map(|r| {
            recursive_max_depth_binary_tree(r.borrow().left.clone())
                .max(recursive_max_depth_binary_tree(r.borrow().right.clone()))
                + 1
        })
        .sum()
}

fn diameter(root: Option<Tree>) -> i64 {
    match root {
        Some(node) => {
            let node_ref = node.borrow();
            let root_diameter = recursive_max_depth_binary_tree(node_ref.left.clone())
                + recursive_max_depth_binary_tree(node_ref.right.clone());
            let left_diameter = diameter(node_ref.left.clone());
            let right_diameter = diameter(node_ref.right.clone());
            root_diameter.max(left_diameter).max(right_diameter)
        }
        None => 0,
    }
}

pub fn main(test_cases: Vec<TreeNumTestCase>) {
    for test_case in test_cases {
        let input: Vec<Option<i64>> = parse_json_array::<i64>(&test_case.input);
        let expected_output: i64 = test_case.output;

        let input_tree = constructor(&input);
        let output: i64 = diameter(input_tree);

        assert_eq!(output, expected_output, "Test Case {:?} failed", input);
    }
    println!("Tests ran successfully for problem ID 543");
}
