#!/usr/bin/env python3
from functools import reduce

def main():
    with open("Day6Input.txt", "r") as file:
        groups = file.read().split("\n\n")

    prob1, prob2 = 0, 0
    for group in groups:
        people = [set(person) for person in group.split("\n") if person != ""]
        union = reduce(lambda a, b: a.union(b), people)
        intersect = reduce(lambda a, b: a.intersection(b), people)
        prob1 += len(union)
        prob2 += len(intersect)
    print(prob1)
    print(prob2)

main()
