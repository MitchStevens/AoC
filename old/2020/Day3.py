#!/usr/bin/env python3
from point import Point

class Forest:
    def __init__(self, filepath):
        with open(filepath, "r") as file:
            lines = file.read().splitlines()
        self.trees = list(map(list, lines))
        self.width = len(self.trees[0])

    def is_tree(self, pos):
        try:
            return self.trees[pos.i][pos.j % self.width] == "#"
        except:
            return False

    def count_trees_on_slope(self, slope):
        pos = slope
        count = 0
        while pos.i <= len(self.trees):
            if self.is_tree(pos):
                count += 1
            pos += slope
        return count


def main():
    forest = Forest("Day3Input.txt")

    # Day 1
    slope = Point(1, 3)
    print(forest.count_trees_on_slope(slope))

    # Day 2
    slopes = [Point(1, 1), Point(1, 3), Point(1, 5), Point(1, 7), Point(2, 1)]
    prod = 1
    for slope in slopes:
        prod *= forest.count_trees_on_slope(slope)
    print(prod)





main()
