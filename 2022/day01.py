from itertools import groupby

def read_input_lines():
    with open("day01.txt", 'r') as f:
        return (x.strip() for x in f.readlines())

def get_calories_list():
    lines = read_input_lines()
    calories_groups = (x for k,x in groupby(lines, lambda c: c == '') if not k)
    int_list = (list(map(int, x)) for x in calories_groups)
    return map(sum, int_list)

def part1():
    calories_list = get_calories_list()
    max_calories = max(calories_list)
    print(max_calories)

def part2():
    calories_list = list(sorted(get_calories_list()))
    max_three = sum(calories_list[-3:])
    print(max_three)


if __name__ == "__main__":
    part1()
    part2()