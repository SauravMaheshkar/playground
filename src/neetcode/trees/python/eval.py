#!/usr/bin/env python

from typing import List
from collections import deque


class Node:
    def __init__(self, val=0, left=None, right=None) -> None:
        self.val = val
        self.left = left
        self.right = right


class Helper:
    def constructor(self, data: List[int]):
        if data == []:
            return None

        root = Node(data[0])

        for value in data[1:]:
            self.insert(root, value)

        return root

    def insert(self, root: Node, value: int) -> None:
        queue = [root]
        while queue:
            node = queue.pop(0)
            if node.left is None:
                node.left = Node(value)
                return
            elif node.right is None:
                node.right = Node(value)
                return
            else:
                queue.append(node.left)
                queue.append(node.right)

    def destructor(self, root: Node) -> List[int]:
        if not root:
            return []

        arr = []
        queue = deque([root])

        while queue:
            node = queue.popleft()
            arr.append(node.val)
            if node.left:
                queue.append(node.left)
            if node.right:
                queue.append(node.right)

        return arr
