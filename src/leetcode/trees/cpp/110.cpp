#include "eval.cpp"
#include <algorithm>
#include <string>

int check_balanced(Node *root) {
  // Empty Tree is balanced
  if (root == nullptr)
    return 0;

  int left_balance = check_balanced(root->left);
  int right_balance = check_balanced(root->right);

  // if either left subTree or right subTree or their difference
  // is more than 1, tree is not balanced
  if (left_balance == -1 || right_balance == -1 ||
      abs(left_balance - right_balance) > 1)
    return -1;

  return max(left_balance, right_balance) + 1;
}

bool is_binary_tree_balanced(Node *root) {
  if (root == nullptr)
    return true;

  if (check_balanced(root) == -1)
    return false;

  return true;
}

int main(int argc, char *argv[]) {
  // Parse arguments
  vector<int> input = parse_single_array(argv[1]);
  string expected_output = bool_to_string(argv[2]);

  // Create Trees and get depth
  Node *inputTree = constructor(input);
  string output = bool_to_string(is_binary_tree_balanced(inputTree));

  if (output != expected_output) {
    printf("Error getting checking if the tree is balanced(110) for provided "
           "input tree\n");
    return 1;
  }

  return 0;
}
