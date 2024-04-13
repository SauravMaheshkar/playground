#!/usr/bin/env python

from typing import Optional

__all__ = ["checkSame"]


class Node:
    def __init__(self, val=0, left=None, right=None) -> None:
        self.val = val
        self.left = left
        self.right = right


def checkSame(x: Optional[Node], y: Optional[Node]) -> bool:
    if not x and not y:
        return True
    if not x or not y:
        return False
    else:
        return (
            (x.val == y.val)
            and (checkSame(x.left, y.left))
            and (checkSame(x.right, y.right))
        )
