#!/usr/bin/env python3

class BoardingPass(object):
    def __init__(self, *args):
        if len(args) == 1:
            boardingPass = args[0]
            self.row = 0
            self.column = 0

            num, i = 0, 0
            for c in boardingPass[0:7][::-1]:
                if c == 'B':
                    self.row += 1 << i
                i += 1

            num, i = 0, 0
            for c in boardingPass[7:10][::-1]:
                if c == 'R':
                    self.column += 1 << i
                i += 1
        elif len(args) == 2:
            self.row = args[0]
            self.column = args[1]

    def __repr__(self):
        return str(self.row) +"/"+ str(self.column)

    def __eq__(self, other):
        return self.row == other.row and self.column == other.column

    def seat_id(self):
        return self.row * 8 + self.column


def main():
    with open("Day5Input.txt", "r") as file:
        lines = file.read().splitlines()

    passes = list(map(BoardingPass, lines))
    seat_ids = list(map(lambda bp: bp.seat_id(), passes))

    print(max(seat_ids))

    for row in range(0, 128):
        for col in range(0, 8):
            bp = BoardingPass(row, col)
            if bp not in passes:
                if bp.seat_id() + 1 in seat_ids and bp.seat_id()-1 in seat_ids:
                    print(bp.seat_id())






main()
