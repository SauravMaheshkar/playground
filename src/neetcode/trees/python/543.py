#!/usr/bin/env python

from typing import Optional

__all__ = ["diameter"]


class Node:
    def __init__(self, val=0, left=None, right=None) -> None:
        self.val = val
        self.left = left
        self.right = right


def maximumDepth(root: Optional[Node]) -> int:
    if root:
        return max(maximumDepth(root.left), maximumDepth(root.right)) + 1
    else:
        return 0


def diameter(root: Optional[Node]) -> int:
    if root:
        root_diameter = maximumDepth(root.left) + maximumDepth(root.right)
        left_diameter = diameter(root.left)
        right_diameter = diameter(root.right)
        return max(root_diameter, left_diameter, right_diameter)
    else:
        return 0
