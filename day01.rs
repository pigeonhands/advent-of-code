use std::{
    io::{self, BufRead},
    fs::File
};

fn read_input() -> Vec<u32> {
    let file = File::open("day01.txt").unwrap();
    io::BufReader::new(file).lines().map(|s| s.unwrap().parse().unwrap()).collect()
}

fn pairs_sum_to(input: &[u32], target_sum: u32) -> (u32,u32) {
    for (i, x) in input.iter().enumerate(){
        for y in input[i..].iter(){
            if (x+y) == target_sum {
                return (*x,*y)
            }
        }
    }
    return (0,0);
}

fn trios_sum_to(input: &[u32], target_sum: u32) -> (u32,u32,u32) {
    for (i, x) in input.iter().enumerate(){
        for y in input[i..].iter(){
            for z in input[i+1..].iter(){
                if (x+y+z) == target_sum {
                    return (*x,*y,*z)
                }
            }
        }
    }
    return (0,0,0);
}

fn main() {
    let input_data = read_input();
    let pair_sum_2020 = pairs_sum_to(&input_data, 2020);
    let multiplied_pair = pair_sum_2020.0 * pair_sum_2020.1;
    println!("part 1 {:?} -> {:}", pair_sum_2020, multiplied_pair);

    let trio_sum_2020 = trios_sum_to(&input_data, 2020);
    let multiplied_trio = trio_sum_2020.0 * trio_sum_2020.1 * trio_sum_2020.2;
    println!("part 2 {:?} -> {:}", trio_sum_2020, multiplied_trio);
}