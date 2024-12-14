#!/usr/bin/env python3

import re

def main():
    with open("Day4Input.txt", "r") as file:
        passports = file.read().split("\n\n")

    fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

    def is_valid(passport):
        for field in fields:
            if field not in passport:
                return False
        return True

    count = 0
    for passport in passports:
        count += (1 if is_valid(passport) else 0)
    print(count)
    print(len(passports))


main()
