#!/usr/bin/env python3

turn = 0
last_spoken = None
#puzzle_input = [8,13,1,0,18,9]
puzzle_input = [0, 3, 6]
#end_turn = 2020
end_turn = 30000000
history = [None] * end_turn

def update_history(n):
    a = history[n]
    if not a:
        history[n] = [turn]
    else:
        history[n] = [turn, a[0]]

for n in puzzle_input:
    update_history(n)
    last_spoken = n
    turn += 1

while turn < end_turn:
    h = history[last_spoken]
    if len(h) == 1:
        next_spoken = 0
    else:
        next_spoken = h[0] - h[1]
    update_history(next_spoken)
    last_spoken = next_spoken
    turn += 1

print(last_spoken)
