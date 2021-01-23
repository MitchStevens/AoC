#!/usr/bin/env python3

class Point:
    def __init__(self, i, j):
        self.i = i
        self.j = j

    def __str__(self):
        return "(" + str(self.i) + ", " + str(self.j) + ")"

    def __add__(self, p):
        return Point(self.i + p.i, self.j + p.j)
