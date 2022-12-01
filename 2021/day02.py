from typing import List

class Password:
    def __init__(self, x, y, letter, password) -> None:
        self.x = x
        self.y = y
        self.letter = letter
        self.password = password

    @classmethod
    def from_str(cls, input: str):
        pos_s, letter, password = input.split()
        xs, ys = pos_s.split('-')
        return cls(int(xs), int(ys), letter[:-1], password)

def read_input() -> List[int]:
    with open('day02.txt', 'r') as f:
        return list(map(Password.from_str, f.readlines()))

def part1() -> int:
    input_data = read_input()
    valid_passwords = [p for p in input_data if p.x <= p.password.count(p.letter) <= p.y]
    return len(valid_passwords)

def part2() -> int:
    input_data = read_input()
    valid_passwords = [p for p in input_data if len({i+1 for i,c in enumerate(p.password) if c==p.letter}.intersection({p.x,p.y})) == 1]
    return len(valid_passwords)

if __name__ == "__main__":
    print("Part1:", part1())
    print("Part2:", part2())

