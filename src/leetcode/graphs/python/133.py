#!/usr/bin/env python
# trivial without a Node(val, outlist) struct

from typing import List

__all__ = ["clone"]


def clone(graph: List[List[int]]) -> List[List[int]]:
    clone = []

    if graph == []:
        return []

    for node in graph:
        clone.append(node)

    return clone
