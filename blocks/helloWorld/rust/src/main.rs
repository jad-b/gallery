#[test]
fn test_hello_world_string() {
    let hw = hello_world_string();
    assert_eq!(hw, "Hello, world!".to_string());
}

fn  hello_world_string() -> String {
    "Hello, world!".to_string()
}

fn main() {
    let s = hello_world_string();
    println!("{}", s);
}
