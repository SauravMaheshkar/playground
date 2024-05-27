use crate::eval::{constructor, Tree, TreeTreeBoolTestCase};
use crate::utils::parse_json_array;

fn check_same(x: Option<Tree>, y: Option<Tree>) -> bool {
    if x.is_none() && y.is_none() {
        return true;
    }
    if x.is_none() || y.is_none() {
        return false;
    }
    if x.as_ref().unwrap().borrow().val != y.as_ref().unwrap().borrow().val {
        return false;
    }
    if check_same(
        x.as_ref().unwrap().borrow().left.clone(),
        y.as_ref().unwrap().borrow().left.clone(),
    ) == false
    {
        return false;
    }
    if check_same(
        x.as_ref().unwrap().borrow().right.clone(),
        y.as_ref().unwrap().borrow().right.clone(),
    ) == false
    {
        return false;
    }

    true
}

pub fn main(test_cases: Vec<TreeTreeBoolTestCase>) {
    for (_, test_case) in test_cases.iter().enumerate() {
        let x_input = parse_json_array::<i64>(&test_case.input_one);
        let y_input = parse_json_array::<i64>(&test_case.input_two);
        let expected_output = test_case.output;

        let x_tree = constructor(&x_input);
        let y_tree = constructor(&y_input);
        let output = check_same(x_tree, y_tree);

        assert_eq!(
            output, expected_output,
            "Test Case {:?} {:?} failed",
            x_input, y_input
        );
    }
    println!("Tests ran successfully for problem ID 100");
}
