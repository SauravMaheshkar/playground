#include "eval.c"
#include "stdbool.h"
#include "stddef.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "sys/param.h"

int check_balanced(Node *root) {
  // Empty Tree is balanced
  if (root == NULL)
    return 0;

  int left_balance = check_balanced(root->left);
  int right_balance = check_balanced(root->right);

  // if either left subTree or right subTree or their difference
  // is more than 1, tree is not balanced
  if (left_balance == -1 || right_balance == -1 ||
      abs(left_balance - right_balance) > 1)
    return -1;

  return MAX(left_balance, right_balance) + 1;
}

bool is_binary_tree_balanced(Node *root) {
  if (root == NULL)
    return true;

  if (check_balanced(root) == -1)
    return false;

  return true;
}

int main(int argc, char *argv[]) {
  // Parse arguments
  int input_size = 0;
  int *input = parse_single_array(argv[1], &input_size);
  char *expected_output = bool_to_string(argv[2]);

  // Create Trees and get depth
  Node *inputTree = constructor(input, input_size);
  char *output = bool_to_string(is_binary_tree_balanced(inputTree));

  if (strcmp(output, expected_output) != 0) {
    printf("Error getting checking if the tree is balanced(110) for provided "
           "input tree\n");
    return 1;
  }

  return 0;
}
