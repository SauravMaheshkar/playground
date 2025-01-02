#!/usr/bin/env python

__all__ = ["course_schedule_two"]

from collections import defaultdict, deque
from typing import List, Optional


def course_schedule_two(
    num_courses: int, prerequisites: List[List[int]]
) -> Optional[List]:
    if prerequisites == [] or prerequisites is None:
        return None

    in_deg = [0] * num_courses
    adj = defaultdict(list)

    # create adjacency list and compute indegrees
    for course, req in prerequisites:
        adj[req].append(course)
        in_deg[course] += 1

    # create dequeue (python's stack) with origin vertices
    queue = deque([i for i in range(num_courses) if in_deg[i] == 0])
    processed = 0
    order = []

    while queue:
        course = queue.popleft()
        processed += 1
        order.append(course)

        for next in adj[course]:
            in_deg[next] -= 1
            if in_deg[next] == 0:
                queue.append(next)

    if processed == num_courses:
        return order
    return None
