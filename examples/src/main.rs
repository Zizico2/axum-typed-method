use axum::routing::MethodFilter;
use axum::Router;
use axum_extra::routing::TypedPath;
use axum_typed_method::RouterExt;
use macros::TypedMethod;
use serde::Deserialize;
use tokio::net::TcpListener;

#[tokio::main]
async fn main() {
    // Register a typed route inferred from `UsersMember` and start serving requests.
    let app = Router::new().typed(get_user);

    let listener = TcpListener::bind("127.0.0.1:3000")
        .await
        .expect("failed to bind 127.0.0.1:3000");

    println!("Listening on http://127.0.0.1:3000");

    axum::serve(listener, app)
        .await
        .expect("server error");
}

async fn get_user(UsersMember { id }: UsersMember) -> String {
    format!("User {id} is ready")
}

#[derive(TypedMethod, TypedPath, Deserialize)]
#[typed_method(MethodFilter::GET)]
#[typed_path("/users/{id}")]
struct UsersMember {
    id: u32,
}
