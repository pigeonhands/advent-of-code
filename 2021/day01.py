from typing import Tuple, List, TypeVar, Iterable, Callable

import math
import itertools


def read_input() -> List[int]:
    with open('day01.txt', 'r') as f:
        return list(map(int, f.readlines()))

def find_subset(data: Iterable[Tuple[int]], f: Callable[[Iterable[int]], int], equals: int) -> Tuple[int]:
    return next((g for g in data if f(g) == equals))

def part1() -> int:
    input_data = read_input()
    subset_2020 = find_subset(itertools.combinations(input_data, 2), sum, 2020)
    return math.prod(subset_2020)

def part2() -> int:
    input_data = read_input()
    subset_2020 = find_subset(itertools.combinations(input_data, 3), sum, 2020)
    return math.prod(subset_2020)

if __name__ == "__main__":
    print('Part1:', part1())
    print('Part2:', part2())
