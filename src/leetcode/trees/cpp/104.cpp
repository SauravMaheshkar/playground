#include "eval.cpp"
#include "stddef.h"
#include <cstdio>
#include <sys/param.h>
#include <vector>

using namespace std;

int recursive_max_depth_binary_tree(Node *root) {
  if (root != nullptr) {
    int leftDepth = recursive_max_depth_binary_tree(root->left);
    int rightDepth = recursive_max_depth_binary_tree(root->right);

    return MAX(leftDepth, rightDepth) + 1;
  }

  return 0;
}

int main(int argc, char *argv[]) {
  // Parse arguments
  vector<int> input = parse_single_array(argv[1]);
  int expected_output = atoi(argv[2]);

  // Create Trees and get depth
  Node *inputTree = constructor(input);
  int output = recursive_max_depth_binary_tree(inputTree);

  if (output != expected_output) {
    printf(
        "Error getting depth(104) for provided input tree expected %d got %d\n",
        expected_output, output);
    return 1;
  }

  return 0;
}
