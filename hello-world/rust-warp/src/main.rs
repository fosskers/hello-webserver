use warp::Filter;

#[tokio::main]
async fn main() {
    let hello = warp::path::end().map(|| format!("Hello, World!"));

    warp::serve(hello).run(([127, 0, 0, 1], 8080)).await;
}
