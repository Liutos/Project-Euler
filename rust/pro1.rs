fn main() {
    let answer = pro1(1000);
    println!("答案为{}", answer);
}

fn pro1(num: i32) -> i32 {
    let mut sum = 0;
    for i in 1..num {
        if i % 3 == 0 || i % 5 == 0 {
            sum += i;
        }
    }
    sum
}
