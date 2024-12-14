#!/usr/bin/env python3

class Direction(Enum):
    UP = 0
    RIGHT = 1
    DOWN = 2
    LEFT =  3


class Goal(Enum):
    Door = auto
    Key = auto


    def door(name):

    def key(name):



class Move:
    def __init__(self, directions):





class NeptunianVault:
    def __init__(self, filepath):
        with open(filepath, "r") as file:
            input = map(list, file.read().splitlines())

        self.keysCollected = []
        self.vault = {}
        self.steps_walked = 0
        self.doors_opened = []

        i = 0, j = 0
        for line in input:
            for char in line:
                self.vault[(i, j)] = char
                i += 1
            j += 1

    def


    def moves(self):
        # what keys can i find
        # what doors can i unlock?

    def _moves_rec(self, directions):
        for d in


    def best_move(self, moves)
