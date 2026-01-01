use macros::TypedMethod;

fn main() {
    println!("Hello, world!");
}
#[derive(TypedMethod)]
#[typed_method(GET)]
struct UsersMember {
    id: u32,
}