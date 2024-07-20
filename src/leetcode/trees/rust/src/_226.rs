use crate::eval::{constructor, destructor, Tree, TreeNode, TreeTreeTestCase};
use crate::utils::parse_json_array;

trait TreeNodeSwap {
    fn swap(&mut self);
    fn swap_all(&mut self);
}

impl TreeNodeSwap for TreeNode {
    fn swap(&mut self) {
        std::mem::swap(&mut self.left, &mut self.right)
    }

    fn swap_all(&mut self) {
        self.left.as_mut().map(|node| node.borrow_mut().swap_all());
        self.right.as_mut().map(|node| node.borrow_mut().swap_all());
        self.swap();
    }
}

fn invert_tree(root: Option<Tree>) -> Option<Tree> {
    root.map(|node| {
        node.borrow_mut().swap_all();
        node
    })
}

pub fn main(test_cases: Vec<TreeTreeTestCase>) {
    for (_, test_case) in test_cases.iter().enumerate() {
        let input = parse_json_array::<i64>(&test_case.input);
        let expected_output = parse_json_array::<i64>(&test_case.output);

        let input_tree = constructor(&input);
        let inverted_tree = invert_tree(input_tree);
        let output = destructor(inverted_tree);

        assert_eq!(output, expected_output, "Test Case {:?} failed", input);
    }
    println!("Tests ran successfully for problem ID 226");
}
