use chrono::prelude::*;
use chrono::Duration;
use hyper::service;
use hyper::{Body, Method, Request, Response, Server, StatusCode};
use serde::{Deserialize, Serialize};
use std::net::SocketAddr;

#[derive(Serialize, Deserialize)]
enum Colour {
    Red,
    Green,
    Blue,
}

#[derive(Deserialize, Serialize)]
struct User<'a> {
    name: &'a str,
    age: u32,
    profile: &'a str,
    colour: Colour,
    numbers: Vec<u32>,
    timestamp: DateTime<Utc>,
    missing: Option<bool>,
}

impl<'a> User<'a> {
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

async fn hello_world(r: Request<Body>) -> Result<Response<Body>, hyper::Error> {
    let (status, body) = match (r.method(), r.uri().path()) {
        (&Method::POST, "/") => {
            let body = hyper::body::to_bytes(r.into_body()).await?;

            match serde_json::from_slice::<User>(&body) {
                Err(_) => (StatusCode::BAD_REQUEST, Body::empty()),
                Ok(mut user) => {
                    user.tweak();
                    let json = serde_json::to_vec(&user).unwrap();
                    (StatusCode::OK, Body::from(json))
                }
            }
        }
        _ => (StatusCode::NOT_FOUND, Body::empty()),
    };

    let response = Response::builder()
        .status(status)
        .header("Content-Type", "application/json")
        .body(body)
        .unwrap();

    Ok(response)
}

#[tokio::main]
async fn main() {
    let addr = SocketAddr::from(([127, 0, 0, 1], 8080));

    let make_svc = service::make_service_fn(|_| async {
        Ok::<_, hyper::Error>(service::service_fn(hello_world))
    });

    let server = Server::bind(&addr).serve(make_svc);

    if let Err(e) = server.await {
        eprintln!("Server error: {}", e);
    }
}
