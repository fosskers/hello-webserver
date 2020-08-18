use actix_web::web::Json;
use actix_web::{post, App, HttpResponse, HttpServer};
use chrono::prelude::*;
use chrono::Duration;
use serde::{Deserialize, Serialize};

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

#[post("/")]
async fn index(item: Json<User>) -> HttpResponse {
    let mut user = item.0;
    user.tweak();
    HttpResponse::Ok().json(user)
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| App::new().service(index))
        .bind("127.0.0.1:8080")?
        .run()
        .await
}
