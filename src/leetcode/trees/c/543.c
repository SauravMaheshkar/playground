#include "eval.c"
#include "stddef.h"
#include "stdio.h"
#include "stdlib.h"
#include "sys/param.h"

int recursive_max_depth_binary_tree(Node *root) {
  if (root != NULL) {
    int leftDepth = recursive_max_depth_binary_tree(root->left);
    int rightDepth = recursive_max_depth_binary_tree(root->right);

    return MAX(leftDepth, rightDepth) + 1;
  }

  return 0;
}

int diameter(Node *root) {
  if (root != NULL) {
    int root_diameter = recursive_max_depth_binary_tree(root->left) +
                        recursive_max_depth_binary_tree(root->right);
    int left_diameter = diameter(root->left);
    int right_diameter = diameter(root->right);

    return variadic_max(3, root_diameter, left_diameter, right_diameter);
  }

  return 0;
}

int main(int argc, char *argv[]) {
  // Parse arguments
  int input_size = 0;
  int *input = parse_single_array(argv[1], &input_size);
  int expected_output = atoi(argv[2]);

  // Create Trees and get depth
  Node *inputTree = constructor(input, input_size);
  int output = diameter(inputTree);

  if (output != expected_output) {
    printf("Error getting diameter(543) for provided input tree expected %d "
           "got %d\n",
           expected_output, output);
    return 1;
  }

  return 0;
}
