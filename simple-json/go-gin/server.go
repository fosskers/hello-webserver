package main

import (
	"fmt"
	"github.com/gin-gonic/gin"
	"time"
)

type User struct {
	Name      string    `json:"name"`
	Age       int       `json:"age"`
	Profile   string    `json:"profile"`
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

func handleUser(c *gin.Context) {
	var user User
	err := c.ShouldBindJSON(&user)
	if err != nil {
		c.String(400, "Bad body")
	} else {
		tweakUser(&user)
		c.JSON(200, user)
	}
}

func main() {
	gin.SetMode(gin.ReleaseMode)
	r := gin.New()
	r.POST("/", handleUser)
	r.Run()
}
