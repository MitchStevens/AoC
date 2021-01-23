#!/usr/bin/env python3

plane = {}
height = None
width = None

with open("Day11Input.txt", "r") as file:
    i, j = 0, 0
    for line in file:
        for seat in list(line):
            plane[(i, j)] = seat
            j += 1
        i += 1
    height = i
    width = j

def adjacent_seats(seat):
    adj = []
    for i in [seat[0]-1, seat[0], seat[0]+1]:
        for j in [seat[1]-1, seat[1], seat[1]+1]:
            if (i, j) != seat:
                adj += plane[(i, j)]
    return adj

def next_seat_state(seat):
    state = plane[seat]
    adj = adjacent_seats(seat)
    if state == 'L':
        if all(map(adj, lambda c: c != '#')):
            return '#'
    elif state == '#':
        if len(filter(adj, lambda c: c == '#')) >= 4:
            return 'L'
    else:
        return plant[seat]

def plane_seats():
    for i in range(0, height):
        for j in range(0, width):
            yield (i, j)

def print_plane():
    for seat in plane_seats:


while True:
    next_plane = {}
    for key in plane:
        next_plane[key] = next
    map(next_seat_state, plane.keys())
    if plane == next_plane:
        break
    plane = next_plane

print(len(list(filter(lambda c: c == '#', plane.values()))))
