fn read_input() -> Vec<String> {
    std::fs::read_to_string("day01.txt")
        .unwrap()
        .lines()
        .map(String::from)
        .collect()
}

fn get_calories_list() -> Vec<i32> {
    let input = read_input();
    let output = input.into_iter().fold(vec![0], |mut acc, x| {
        if x == "" {
            acc.push(0);
        } else {
            *acc.last_mut().unwrap() += x.parse::<i32>().unwrap();
        }
        acc
    });
    output
}

fn part1() -> i32 {
    let calories_list = get_calories_list();
    calories_list.into_iter().max().unwrap()
}

fn part2() -> i32 {
    let mut calories_list = get_calories_list();
    calories_list.sort();
    calories_list.into_iter().rev().take(3).sum()
}

fn main() {
    println!("{}", part1());
    println!("{}", part2());
}
