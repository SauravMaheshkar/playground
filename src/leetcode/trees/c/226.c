#include "eval.c"
#include "stddef.h"

struct Node *invertTree(struct Node *root) {
  if (root != NULL) {
    // Swap Left and Right subTree
    struct Node *tmp = root->left;
    root->left = root->right;
    root->right = tmp;
    // Recursively invert Left and Right subTree
    invertTree(root->left);
    invertTree(root->right);
  }

  return root;
}

int main(int argc, char *argv[]) {
  // Parse arguments
  int output_size = 0, input_size = 0, expected_output_size = 0;
  int *input = parse_single_array(argv[1], &input_size);
  int *expected_output = parse_single_array(argv[2], &expected_output_size);

  // Create Trees
  Node *inputTree = constructor(input, input_size);
  Node *invertedTree = invertTree(inputTree);

  if (invertedTree == 0) {
    int *output = (int *)malloc(0);
    return 0;
  }

  // Convert Node* to int*
  int *output = destructor(invertedTree, &output_size);
  if (arrcmp(output, expected_output, output_size, expected_output_size) != 0) {
    return 1;
  }

  return 0;
}
