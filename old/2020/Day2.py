#!/usr/bin/env python3

class Rule:
    def __init__(self, char, lo, hi):
        self.char = char
        self.lo = lo
        self.hi = hi

    def is_valid(self, string):
        count = 0
        for char in string:
            if char == self.char:
                count += 1

        return self.lo <= count <= self.hi

    def parse(string):
        [range, char] = string.split(" ")
        [lo, hi] = range.split("-")
        return Rule(list(char.strip())[0], int(lo.strip()), int(hi.strip()))


class NewRule:
    def __init__(self, char, index1, index2):
        self.char = char
        self.index1 = index1
        self.index2 = index2

    def is_valid(self, string):
        def test_index(index):
            return list(string)[index] == self.char

        try:
            return test_index(self.index1) ^ test_index(self.index2)
        except IndexError:
            return False

    def parse(string):
        [indices, char] = string.split(" ")
        [index1, index2] = indices.split("-")
        return NewRule(list(char.strip())[0], int(index1.strip())-1, int(index2.strip())-1)


def main():
    with open("Day2Input.txt", "r") as file:
        lines = file.read().splitlines()

    count1, count2 = 0, 0
    for line in lines:
        [rule, password] = line.split(":")
        if Rule.parse(rule).is_valid(password):
            count1 += 1
        if NewRule.parse(rule).is_valid(password.strip()):
            count2 += 1

    print(count1)
    print(count2)

main()
