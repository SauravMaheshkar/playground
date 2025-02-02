#!/usr/bin/env python

from typing import List

__all__ = ["biggest_island"]


def traverse(grid: List[List[int]], i: int, j: int) -> None:
    if (
        (i < 0)
        or (j < 0)
        or (i >= len(grid))
        or (j >= len(grid[0]))
        or (grid[i][j] != 1)
    ):
        return 0

    grid[i][j] = 0
    return (
        1
        + traverse(grid, i + 1, j)
        + traverse(grid, i - 1, j)
        + traverse(grid, i, j + 1)
        + traverse(grid, i, j - 1)
    )


def biggest_island(grid: List[List[int]]) -> int:
    max_area = 0

    for i in range(len(grid)):
        for j in range(len(grid[0])):
            if grid[i][j] == 1:
                max_area = max(max_area, traverse(grid, i, j))

    return max_area
