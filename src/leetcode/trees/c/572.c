#include "eval.c"
#include "stdbool.h"
#include "stddef.h"
#include "stdio.h"
#include "string.h"

bool check_same(Node *x, Node *y) {
  if (!x && !y)
    return true;
  if (!x || !y)
    return false;
  if (x->val != y->val)
    return false;
  if (check_same(x->left, y->left) == false)
    return false;
  if (check_same(x->right, y->right) == false)
    return false;

  return true;
}

bool is_subtree(Node *x, Node *y) {
  if (!x || !y)
    return false;

  if (check_same(x, y))
    return true;

  return check_same(x->left, y) || check_same(x->right, y);
}

int main(int argc, char *argv[]) {
  // Parse arguments
  int x_size = 0, y_size = 0;
  int *x_array = parse_single_array(argv[1], &x_size);
  int *y_array = parse_single_array(argv[2], &y_size);

  // Create Trees and get depth
  Node *x_tree = constructor(x_array, x_size);
  Node *y_tree = constructor(y_array, y_size);
  char *output = bool_to_string(is_subtree(x_tree, y_tree));

  if (strcmp(output, argv[3]) != 0) {
    printf("Error getting checking if trees are the same (572) for provided "
           "input tree\n");
    return 1;
  }

  return 0;
}
