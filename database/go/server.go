package main

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"time"

	_ "github.com/mattn/go-sqlite3"
)

type User struct {
	Name      string    `json:"name"`
	Age       int       `json:"age"`
	Profile   string    `json:"profile"`
	Colour    string    `json:"colour"`
	Timestamp time.Time `json:"timestamp"`
	Missing   *bool     `json:"missing,omitEmpty"`
}

func intToColour(i int) *string {
	var res string
	if i == 0 {
		res = "Red"
	} else if i == 1 {
		res = "Green"
	} else if i == 2 {
		res = "Blue"
	} else {
		return nil
	}
	return &res
}

func handler(db *sql.DB, w http.ResponseWriter, r *http.Request) {
	if r.Method != "GET" {
		http.Error(w, "Method not allowed", 405)
		return
	}

	name := r.URL.Path[1:]
	user, err := userLookup(db, name)
	if err != nil {
		http.Error(w, "Invalid user name", 400)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(user)
}

func userLookup(db *sql.DB, u string) (*User, error) {
	var name string
	var age int
	var profile string
	var colour int
	var timestamp string
	var missing *bool

	q := db.QueryRow("select * from test where name = ? limit 1", u)
	err := q.Scan(&name, &age, &profile, &colour, &timestamp, &missing)
	if err != nil {
		return nil, err
	}

	realColour := intToColour(colour)
	if realColour == nil {
		return nil, fmt.Errorf("Invalid colour value: %d", colour)
	}

	t, err := time.Parse(time.RFC3339, timestamp)
	if err != nil {
		return nil, err
	}

	user := &User{
		Name:      name,
		Age:       age,
		Profile:   profile,
		Missing:   missing,
		Colour:    *realColour,
		Timestamp: t,
	}

	return user, nil
}

func main() {
	db, _ := sql.Open("sqlite3", "../test.db")
	defer db.Close()

	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		handler(db, w, r)
	})
	log.Fatal(http.ListenAndServe(":8080", nil))
}
