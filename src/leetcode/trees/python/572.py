#!/usr/bin/env python

from typing import Optional


__all__ = ["isSubtree"]


class Node:
    def __init__(self, val=0, left=None, right=None) -> None:
        self.val = val
        self.left = left
        self.right = right


def isSame(x: Optional[Node], y: Optional[Node]) -> bool:
    if not x and not y:
        return True
    if not x or not y:
        return False
    else:
        return (
            (x.val == y.val) and (isSame(x.left, y.left)) and (isSame(x.right, y.right))
        )


def isSubtree(root: Optional[Node], subTree: Optional[Node]) -> bool:
    if subTree is None:
        return True

    if root is None:
        return False

    if isSame(root, subTree):
        return True

    return isSubtree(root.left, subTree) or isSubtree(root.right, subTree)
