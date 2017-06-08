#[test]
fn test_hello_world_string() {
    let hw = hello_world_string();
    assert_eq!(hw, "Hello, world!".to_string());
}

fn  hello_world_string() -> String {
    "Hello, world!".to_string()
}

fn print_hello_world() {
    let s = hello_world_string();
    println!("{}", s);
}
