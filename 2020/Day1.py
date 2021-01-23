

def find_sum_2(numbers, target):
    for a in iter(numbers):
        for b in iter(numbers):
            if a+b == target:
                return a*b


def find_sum_3(numbers, target):
    for a in numbers:
        for b in numbers:
            for c in numbers:
                if a+b+c == target:
                    return a*b*c

def main():
    with open("Day1Input.txt", "r") as file:
        numbers = sorted(list(map(int, (file.read().splitlines()))))
    print(find_sum_2(numbers, 2020))
    print(find_sum_3(numbers, 2020))

main()
