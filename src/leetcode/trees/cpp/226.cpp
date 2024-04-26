#include "eval.cpp"
#include "stddef.h"
#include <vector>

using namespace std;

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
  vector<int> input = parse_single_array(argv[1]);
  vector<int> expected_output = parse_single_array(argv[2]);

  // Create Trees
  Node *inputTree = constructor(input);
  Node *invertedTree = invertTree(inputTree);

  if (invertedTree == nullptr) {
    // Handle the case where invertedTree is null
    vector<int> output = vector<int>();
    if (arrcmp(output, expected_output) != 0) {
      return 1;
    }
    return 0; // or whatever you want to return
  }

  vector<int> output = destructor(invertedTree);
  if (arrcmp(output, expected_output) != 0) {
    return 1;
  }

  return 0;
}
