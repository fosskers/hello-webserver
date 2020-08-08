use chrono::prelude::*;
use chrono::Duration;
use serde::{Deserialize, Serialize};
use warp::Filter;

#[derive(Serialize, Deserialize)]
enum Colour {
    Red,
    Green,
    Blue,
}

#[derive(Deserialize, Serialize)]
struct User {
    name: String,
    age: u32,
    profile: String,
    colour: Colour,
    numbers: Vec<u32>,
    timestamp: DateTime<Utc>,
    missing: Option<bool>,
}

impl User {
    fn tweak(&mut self) {
        self.age += 1;
        self.numbers.iter_mut().for_each(|n| *n *= 3);
        self.timestamp = self.timestamp + Duration::days(1);
        self.missing = Some(true);

        if let Colour::Blue = self.colour {
            self.colour = Colour::Red;
        }
    }
}

#[tokio::main]
async fn main() {
    let handle = warp::post().and(warp::body::json()).map(|mut user: User| {
        user.tweak();
        warp::reply::json(&user)
    });

    warp::serve(handle).run(([127, 0, 0, 1], 8080)).await;
}
