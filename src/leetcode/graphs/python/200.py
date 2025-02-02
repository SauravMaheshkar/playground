#!/usr/bin/env python

from typing import List

__all__ = ["num_islands"]


def traverse(grid: List[List[int]], i: int, j: int) -> None:
    if (
        (i < 0)
        or (j < 0)
        or (i >= len(grid))
        or (j >= len(grid[0]))
        or (grid[i][j] != 1)
    ):
        return

    grid[i][j] = 0
    traverse(grid, i + 1, j)
    traverse(grid, i - 1, j)
    traverse(grid, i, j + 1)
    traverse(grid, i, j - 1)


def num_islands(grid: List[List[int]]) -> int:
    count = 0

    for i in range(len(grid)):
        for j in range(len(grid[0])):
            if grid[i][j] == 1:
                count += 1
                traverse(grid, i, j)

    return count
