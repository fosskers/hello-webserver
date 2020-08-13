use chrono::prelude::*;
use chrono::Duration;
use serde::{Deserialize, Serialize};
use tide::{Body, Request};

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

#[async_std::main]
async fn main() -> Result<(), std::io::Error> {
    let mut app = tide::new();

    app.at("/").post(|mut req: Request<()>| async move {
        let mut user: User = req.body_json().await?;
        user.tweak();
        Ok(Body::from_json(&user)?)
    });

    app.listen("127.0.0.1:8080").await?;
    Ok(())
}
