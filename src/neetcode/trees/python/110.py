#!/usr/bin/env python

from typing import Optional

__all__ = ["balancedBinary"]


class Node:
    def __init__(self, val=0, left=None, right=None) -> None:
        self.val = val
        self.left = left
        self.right = right


def balancedBinary(root: Optional[Node]) -> bool:
    def checkBalance(root: Optional[Node]) -> int:
        if root is None:
            return True

        left = checkBalance(root.left)
        right = checkBalance(root.right)
        if left == -1 or right == -1:
            return -1
        if abs(left - right) > 1:
            return -1
        return max(left, right) + 1

    return checkBalance(root) != -1
