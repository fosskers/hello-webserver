use chrono::prelude::*;
use r2d2::Pool;
use r2d2_sqlite::SqliteConnectionManager;
use rusqlite::params;
use serde::Serialize;
use tide::{Body, Request};

// #[derive(Serialize)]
// #[repr(i32)]
// enum Colour {
//     Red = 0,
//     Green = 1,
//     Blue = 2,
// }

#[derive(Serialize)]
struct User {
    name: String,
    age: i32,
    profile: String,
    // colour: Colour,
    timestamp: DateTime<Utc>,
    missing: Option<bool>,
}

#[derive(Clone)]
struct State {
    pool: Pool<SqliteConnectionManager>,
}

#[async_std::main]
async fn main() -> anyhow::Result<()> {
    let state = {
        let m = SqliteConnectionManager::file("../test.db");
        let pool = Pool::new(m)?;
        State { pool }
    };

    let mut app = tide::with_state(state);
    app.at("/:name").get(|req: Request<State>| async move {
        let conn = req.state().pool.get()?;
        let name: String = req.param("name")?;
        // TODO Try `prepare_cached` too.
        let mut stmt = conn.prepare("SELECT * FROM test WHERE name = ? limit 1")?;
        let user = stmt.query_row(params![name], |row| {
            let user = User {
                name: row.get(0)?,
                age: row.get(1)?,
                profile: row.get(2)?,
                timestamp: row.get(4)?,
                missing: row.get(5)?,
            };
            Ok(user)
        })?;

        Ok(Body::from_json(&user).expect("Json encoding failed"))
    });
    app.listen("127.0.0.1:8080").await?;

    Ok(())
}
