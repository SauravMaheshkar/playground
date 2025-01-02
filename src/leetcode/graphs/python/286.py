#!/usr/bin/env python

from typing import List
from collections import deque


__all__ = ["islandsAndTreasures"]


def islandsAndTreasures(grid: List[List[int]]) -> List[List[int]]:
    ROWS, COLS = len(grid), len(grid[0])
    visit = set()
    q = deque()

    def addCell(r, c):
        if (
            min(r, c) < 0
            or r == ROWS
            or c == COLS
            or (r, c) in visit
            or grid[r][c] == -1
        ):
            return
        visit.add((r, c))
        q.append([r, c])

    for r in range(ROWS):
        for c in range(COLS):
            if grid[r][c] == 0:
                q.append([r, c])
                visit.add((r, c))

    dist = 0
    while q:
        for i in range(len(q)):
            r, c = q.popleft()
            grid[r][c] = dist
            addCell(r + 1, c)
            addCell(r - 1, c)
            addCell(r, c + 1)
            addCell(r, c - 1)
        dist += 1

    return grid
