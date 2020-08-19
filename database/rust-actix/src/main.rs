use actix_web::web::{Data, Path};
use actix_web::{get, App, Error, HttpResponse, HttpServer};
use chrono::prelude::*;
use r2d2::Pool;
use r2d2_sqlite::SqliteConnectionManager;
use rusqlite::params;
use rusqlite::types::{FromSql, FromSqlError, FromSqlResult, ValueRef};
use serde::Serialize;

#[derive(Serialize)]
enum Colour {
    Red,
    Green,
    Blue,
}

impl FromSql for Colour {
    fn column_result(value: ValueRef<'_>) -> FromSqlResult<Self> {
        i64::column_result(value).and_then(|i| {
            if i == 0 {
                Ok(Colour::Red)
            } else if i == 1 {
                Ok(Colour::Green)
            } else if i == 2 {
                Ok(Colour::Blue)
            } else {
                Err(FromSqlError::OutOfRange(i))
            }
        })
    }
}

#[derive(Serialize)]
struct User {
    name: String,
    age: u32,
    profile: String,
    colour: Colour,
    timestamp: DateTime<Utc>,
    missing: Option<bool>,
}

#[get("/{name}")]
async fn get_user(
    Path(name): Path<String>,
    pool: Data<Pool<SqliteConnectionManager>>,
) -> Result<HttpResponse, Error> {
    let conn = pool
        .get()
        .map_err(|_| HttpResponse::InternalServerError())?;
    let mut stmt = conn
        .prepare_cached("SELECT * FROM test WHERE name = ? limit 1")
        .map_err(|_| HttpResponse::InternalServerError())?;
    let user = stmt
        .query_row(params![name], |row| {
            let user = User {
                name: row.get("name")?,
                age: row.get("age")?,
                profile: row.get("profile")?,
                colour: row.get("colour")?,
                timestamp: row.get("timestamp")?,
                missing: row.get("missing")?,
            };
            Ok(user)
        })
        .map_err(|_| HttpResponse::InternalServerError())?;

    Ok(HttpResponse::Ok().json(user))
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    let m = SqliteConnectionManager::file("../test.db");
    let p = Pool::new(m).unwrap();

    HttpServer::new(move || App::new().data(p.clone()).service(get_user))
        .bind("127.0.0.1:8080")?
        .run()
        .await
}
