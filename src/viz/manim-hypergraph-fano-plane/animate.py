# /// script
# requires-python = ">=3.10"
# dependencies = [
#     "manim",
# ]
# ///

from manim import (
    DOWN,
    Circle,
    Create,
    Dot,
    Line,
    Scene,
    Text,
    config,
    line_intersection,
    midpoint,
)

config.frame_width = 32


class FanoPlane(Scene):
    def construct(self):
        ## Create the points
        points = [
            Dot(point=[0, -3, 0], radius=0.2),  # mid-point of bottom line
            Dot(point=[-4, -3, 0], radius=0.2),  # left vertex
            Dot(point=[4, -3, 0], radius=0.2),  # right vertex
            Dot(point=[0, 5, 0], radius=0.2),  # top vertex
            Dot(point=midpoint([0, 5, 0], [-4, -3, 0]), radius=0.2),  # left mid-point
            Dot(point=midpoint([0, 5, 0], [4, -3, 0]), radius=0.2),  # right mid-point
            Dot(
                point=line_intersection(
                    line1=[[0, 5, 0], [0, -3, 0]],
                    line2=[midpoint([0, 5, 0], [-4, -3, 0]), [4, -3, 0]],
                ),
                radius=0.2,
            ),
        ]

        ## Add points to the scene
        for point in points:
            self.play(Create(point))

        text = Text(text="Fano Plane").next_to(points[0], DOWN, buff=1.0)
        self.play(Create(text))

        ## Draw lines
        lines = [
            Line(points[1].get_center(), points[3].get_center()),  # left to top
            Line(points[3].get_center(), points[2].get_center()),  # top to right
            Line(points[2].get_center(), points[1].get_center()),  # right to left
            Line(points[0].get_center(), points[3].get_center()),  # top to mid-bottom
            Line(points[4].get_center(), points[2].get_center()),  # right to mid-left
            Line(points[5].get_center(), points[1].get_center()),  # left to mid-right
        ]
        for line in lines:
            self.play(Create(line))

        ## Draw the circle passing through the midpoints
        circle = Circle.from_three_points(
            p1=points[0].get_center(),
            p2=points[4].get_center(),
            p3=points[5].get_center(),
            color="WHITE",
        )
        self.play(Create(circle))
        self.wait(2)
