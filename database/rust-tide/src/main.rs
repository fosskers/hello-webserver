use chrono::prelude::*;
use serde::Serialize;
use sqlx::SqlitePool;
use tide::{Body, Request};

#[derive(Serialize, sqlx::Type)]
#[repr(i32)]
enum Colour {
    Red = 0,
    Green = 1,
    Blue = 2,
}

#[derive(Serialize, sqlx::FromRow)]
struct User {
    name: String,
    age: i32,
    profile: String,
    colour: Colour,
    // timestamp: DateTime<Utc>,
    missing: Option<bool>,
}

#[derive(Clone)]
struct State {
    pool: SqlitePool,
}

#[async_std::main]
async fn main() -> anyhow::Result<()> {
    let state = {
        let pool = SqlitePool::connect("../test.db").await?;
        State { pool }
    };

    let mut app = tide::with_state(state);
    app.at("/:name").get(|req: Request<State>| async move {
        let name: String = req.param("name")?;
        let pool = &req.state().pool;
        let user = sqlx::query_as::<_, User>("SELECT * from test where name = ? limit 1")
            .bind(name)
            .fetch_one(pool)
            .await?;

        Ok(Body::from_json(&user)?)
    });
    app.listen("127.0.0.1:8080").await?;

    Ok(())
}
