use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;

use serde::Deserialize;
use serde_json::Value;

#[derive(Debug, Deserialize)]
pub struct TreeTreeTestCase {
    pub input: Vec<Value>,
    pub output: Vec<Value>,
}

#[derive(Debug, Deserialize)]
pub struct TreeNumTestCase {
    pub input: Vec<Value>,
    pub output: i64,
}

#[derive(Debug, Deserialize)]
pub struct TreeBoolTestCase {
    pub input: Vec<Value>,
    pub output: bool,
}

#[derive(Debug, Deserialize)]
pub struct TreeTreeBoolTestCase {
    pub input_one: Vec<Value>,
    pub input_two: Vec<Value>,
    pub output: bool,
}

#[derive(Debug)]
pub enum TestCase {
    TreeTreeTestCase(Vec<TreeTreeTestCase>),
    TreeNumTestCase(Vec<TreeNumTestCase>),
    TreeBoolTestCase(Vec<TreeBoolTestCase>),
    TreeTreeBoolTestCase(Vec<TreeTreeBoolTestCase>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct TreeNode {
    pub val: i64,
    pub left: Option<Rc<RefCell<TreeNode>>>,
    pub right: Option<Rc<RefCell<TreeNode>>>,
}

pub type Tree = Rc<RefCell<TreeNode>>;

pub fn convert_test_cases(test_type: &str, file_content: &str) -> TestCase {
    match test_type {
        "TreeTree" => {
            let test_cases: Vec<TreeTreeTestCase> = serde_json::from_str(file_content)
                .expect("Failed to parse JSON for TreeTreeTestCase");
            TestCase::TreeTreeTestCase(test_cases)
        }
        "TreeNum" => {
            let test_cases: Vec<TreeNumTestCase> = serde_json::from_str(file_content)
                .expect("Failed to parse JSON for TreeNumTestCase");
            TestCase::TreeNumTestCase(test_cases)
        }
        "TreeBool" => {
            let test_cases: Vec<TreeBoolTestCase> = serde_json::from_str(file_content)
                .expect("Failed to parse JSON for TreeBoolTestCase");
            TestCase::TreeBoolTestCase(test_cases)
        }
        "TreeTreeBool" => {
            let test_cases: Vec<TreeTreeBoolTestCase> = serde_json::from_str(file_content)
                .expect("Failed to parse JSON for TreeBoolTestCase");
            TestCase::TreeTreeBoolTestCase(test_cases)
        }
        _ => panic!("No module found for test type: {}", test_type),
    }
}

impl TreeNode {
    #[inline]
    pub fn new(val: i64) -> Self {
        TreeNode {
            val,
            left: None,
            right: None,
        }
    }
}

fn insert(root: &Tree, value: Option<i64>) {
    let mut queue = VecDeque::new();
    queue.push_back(root.clone());

    while let Some(node_rc) = queue.pop_front() {
        let mut node = node_rc.borrow_mut();

        if node.left.is_none() {
            if let Some(value) = value {
                node.left = Some(Rc::new(RefCell::new(TreeNode::new(value))));
            }
            return;
        } else {
            queue.push_back(node.left.as_ref().unwrap().clone());
        }

        if node.right.is_none() {
            if let Some(value) = value {
                node.right = Some(Rc::new(RefCell::new(TreeNode::new(value))));
            }
            return;
        } else {
            queue.push_back(node.right.as_ref().unwrap().clone());
        }
    }
}

pub fn constructor(data: &[Option<i64>]) -> Option<Tree> {
    if data.is_empty() {
        return None;
    }

    let root = Rc::new(RefCell::new(TreeNode::new(data[0].unwrap())));

    for &value in &data[1..] {
        insert(&root.clone(), value);
    }

    Some(root)
}

pub fn destructor(root: Option<Tree>) -> Vec<Option<i64>> {
    if let Some(root) = root {
        let mut arr = Vec::new();
        let mut queue = VecDeque::new();
        queue.push_back(root);

        while let Some(node_rc) = queue.pop_front() {
            let node = node_rc.borrow();
            arr.push(Some(node.val));

            if let Some(left) = &node.left {
                queue.push_back(left.clone());
            }
            if let Some(right) = &node.right {
                queue.push_back(right.clone());
            }
        }

        arr
    } else {
        vec![]
    }
}
