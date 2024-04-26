#include <cstdlib>
#include <cstring>
#include <queue>
#include <vector>

#define MAX_SIZE 100

using namespace std;

class Node {
public:
  int val;
  Node *left;
  Node *right;

  // Constructor
  Node(int value) : val(value), left(nullptr), right(nullptr) {}
};

void insert(Node *root, int value) {
  queue<Node *> queue;
  queue.push(root);

  while (!queue.empty()) {
    Node *node = queue.front();
    queue.pop();
    if (node->left == nullptr) {
      node->left = new Node(value);
      return;
    } else if (node->right == nullptr) {
      node->right = new Node(value);
      return;
    } else {
      queue.push(node->left);
      queue.push(node->right);
    }
  }
}

Node *constructor(const vector<int> &data) {
  if (data.empty())
    return nullptr;

  Node *root = new Node(data[0]);

  for (size_t i = 1; i < data.size(); ++i)
    insert(root, data[i]);

  return root;
}

vector<int> destructor(Node *root) {
  vector<int> arr;

  if (root == nullptr) {
    return arr;
  }

  queue<Node *> queue;
  queue.push(root);

  while (!queue.empty()) {
    Node *node = queue.front();
    queue.pop();
    arr.push_back(node->val);

    if (node->left)
      queue.push(node->left);
    if (node->right)
      queue.push(node->right);

    if (arr.size() >= MAX_SIZE - 1) {
      return std::vector<int>(); // Return an empty vector to indicate failure
    }
  }

  return arr;
}

vector<int> parse_single_array(char *buffer) {
  vector<int> data;

  int idx = 0;
  idx++; // Skip over first "["

  // In case of an empty list, return an empty vector
  if (strcmp(&buffer[idx], "]") == 0) {
    return data;
  }

  // Determine the size of the array
  while (strcmp(&buffer[idx], "]") != 0) {
    if (isdigit(buffer[idx])) {
      data.push_back(atoi(&buffer[idx]));
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

int arrcmp(const vector<int> &x, const vector<int> &y) {
  if (x.size() != y.size()) {
    return 1;
  }

  for (size_t i = 0; i < x.size(); i++) {
    if (x[i] != y[i]) {
      return 1;
    }
  }

  return 0;
}

string bool_to_string(bool boolean) {
  if (boolean) {
    return "true";
  } else {
    return "false";
  }
}
