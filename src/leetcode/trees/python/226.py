#!/usr/bin/env python

from typing import Optional

__all__ = ["invertTree"]


class Node:
    def __init__(self, val=0, left=None, right=None) -> None:
        self.val = val
        self.left = left
        self.right = right


def invertTree(root: Optional[Node]) -> Optional[Node]:
    if root:
        root.left, root.right = root.right, root.left
        invertTree(root.left)
        invertTree(root.right)

    return root
