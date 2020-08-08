package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"time"
)

type User struct {
	Name      string    `json:"name"`
	Age       int       `json:"age"`
	Colour    string    `json:"colour"`
	Numbers   []int     `json:"numbers"`
	Timestamp time.Time `json:"timestamp"`
	Missing   *bool     `json:"missing,omitEmpty"`
}

func tweakUser(u *User) error {
	if u == nil {
		return fmt.Errorf("Null user")
	}

	u.Age += 1

	if u.Colour == "Blue" {
		u.Colour = "Red"
	}

	for i := range u.Numbers {
		u.Numbers[i] *= 3
	}

	u.Timestamp = u.Timestamp.AddDate(0, 0, 1)

	itsTrue := true
	u.Missing = &itsTrue

	return nil
}

func handler(w http.ResponseWriter, r *http.Request) {
	if r.Method != "POST" {
		http.Error(w, "Method not allowed", 405)
		return
	}

	decoder := json.NewDecoder(r.Body)
	var user User
	err := decoder.Decode(&user)
	if err != nil {
		http.Error(w, "Unable to decode JSON", 400)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	tweakUser(&user)
	json.NewEncoder(w).Encode(user)
}

func main() {
	http.HandleFunc("/", handler)
	log.Fatal(http.ListenAndServe(":8080", nil))
}
