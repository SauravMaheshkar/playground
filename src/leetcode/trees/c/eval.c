#include "ctype.h"
#include "stdarg.h"
#include "stdbool.h"
#include "stddef.h"
#include "stdlib.h"
#include "string.h"

#define MAX_SIZE 100

typedef struct Node {
  int val;
  struct Node *left;
  struct Node *right;
} Node;

void insert(Node *root, int value) {
  Node *queue[MAX_SIZE];
  int front = -1, rear = -1;
  queue[++rear] = root;

  while (front != rear) {
    Node *node = queue[++front];
    if (node->left == NULL) {
      node->left = (Node *)malloc(sizeof(Node));
      node->left->val = value;
      node->left->left = NULL;
      node->left->right = NULL;
      return;
    } else if (node->right == NULL) {
      node->right = (Node *)malloc(sizeof(Node));
      node->right->val = value;
      node->right->left = NULL;
      node->right->right = NULL;
      return;
    } else {
      queue[++rear] = node->left;
      queue[++rear] = node->right;
    }
  }
}

Node *constructor(int *data, int size) {
  if (size == 0)
    return NULL;

  Node *root = (Node *)malloc(sizeof(Node));
  root->val = data[0];
  root->left = NULL;
  root->right = NULL;

  for (int i = 1; i < size; i++)
    insert(root, data[i]);
  return root;
}

int *destructor(Node *root, int *size) {
  if (root == NULL)
    return 0;

  int *arr = (int *)malloc(MAX_SIZE * sizeof(int));
  if (arr == NULL)
    return NULL;
  int arr_index = 0;

  Node *queue[MAX_SIZE];
  int front = -1, rear = -1;
  queue[++rear] = root;

  while (front != rear) {
    Node *node = queue[++front];
    arr[arr_index++] = node->val;
    if (node->left)
      queue[++rear] = node->left;
    if (node->right)
      queue[++rear] = node->right;

    if (arr_index >= MAX_SIZE - 1) {
      free(arr);
      return NULL;
    }
  }

  *size = arr_index;
  return arr;
}

int *parse_single_array(char *buffer, int *size) {
  int idx = 0;
  idx++; // Skip over fist "["

  // In case of a empty list, return a empty array
  if (strcmp(&buffer[idx], "]") == 0) {
    *size = 0;
    return (int *)malloc(0);
  }

  // Determine size of the array
  while (strcmp(&buffer[idx], "]") != 0) {
    if (isdigit(buffer[idx])) {
      ++(*size);
    }
    idx++;
  }

  int *data = (int *)malloc(*size * sizeof(int));

  idx = 1;
  int data_idx = 0;
  while (strcmp(&buffer[idx], "]") != 0) {
    if (isdigit(buffer[idx])) {
      data[data_idx++] = atoi(&buffer[idx]);
      // Skip consecutive digits
      while (isdigit(buffer[idx])) {
        idx++;
      }
    } else {
      idx++;
    }
  }

  return data;
}

int arrcmp(int *x, int *y, int x_size, int y_size) {
  if (x_size != y_size) {
    return 1;
  }

  for (int i = 0; i < x_size; i++) {
    if (x[i] != y[i])
      return 1;
  }

  return 0;
}

int variadic_max(int count, ...) {
  va_list args;
  va_start(args, count);

  int max = va_arg(args, int);
  for (int i = 1; i < count; i++) {
    int num = va_arg(args, int);
    if (num > max) {
      max = num;
    }
  }

  va_end(args);
  return max;
}

char *bool_to_string(bool boolean) {
  if (boolean == true) {
    return "true\0";
  } else if (boolean == false) {
    return "false\0";
  } else {
    return "invalid\0";
  }
}
