#!/usr/bin/env python

from typing import Optional

__all__ = ["recursive_max_depth_binary_tree"]


class Node:
    def __init__(self, val=0, left=None, right=None) -> None:
        self.val = val
        self.left = left
        self.right = right


def recursive_max_depth_binary_tree(root: Optional[Node]) -> int:
    if root:
        return (
            max(
                recursive_max_depth_binary_tree(root.left),
                recursive_max_depth_binary_tree(root.right),
            )
            + 1
        )
    else:
        return 0
