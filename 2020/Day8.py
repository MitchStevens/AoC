#!/usr/bin/env python3

def run_program(program):
    acc = 0
    instructions = set()
    curr = 0
    while curr not in instructions:
        if curr == len(program):
            return acc
        instructions.add(curr)
        if program[curr][0:3] == "nop":
            curr += 1
        elif program[curr][0:3] == "acc":
            acc += int(program[curr][4:])
            curr += 1
        elif program[curr][0:3] == "jmp":
            curr += int(program[curr][4:])
    return acc

def halts(program):
    instructions = set()
    curr = 0
    while curr not in instructions:
        if curr == len(program):
            return True
        instructions.add(curr)
        if program[curr][0:3] == "jmp":
            curr += int(program[curr][4:])
        else:
            curr += 1

    return False

a = lambda a b c: a and b or c
#a = lambda a b c: a and b or c

def augment_program(program):
    for n in range(0, len(program)-1):
        if program[n][0:3] == "jmp":
            program[n] = "nul" + program[n][3:]
            yield program.copy()
            program[n] = "jmp" + program[n][3:]
        elif program[n][0:3] == "nul":
            program[n] = "jmp" + program[n][3:]
            yield program.copy()
            program[n] = "nul" + program[n][3:]

with open("Day8Input.txt", "r") as file:
    lines = file.read().splitlines()

print(run_program(lines))

for program in augment_program(lines):
    if halts(program):
        print(run_program(program))
