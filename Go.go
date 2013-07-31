package main

import (
	"flag"
	"fmt"
	"time"
)

const (
	TileDim = 50
	MinWid  = 2
	MaxWid  = 8
)

type Tile struct {
	X int32
	Y int32
	T int32
}

type Room struct {
	X int32
	Y int32
	W int32
	H int32
	N int32
}

type Lev struct {
	ts []Tile
	rs []Room
}

func GenRand(gen *uint32) int32 {
	seed := (*gen << 1) + 1
	if int32(seed) < 0 {
		seed ^= 0x88888eef
	}
	*gen = seed
	return int32(seed)
}

func CheckColl(x, y, w, h int32, rs []Room) bool {
	var r *Room
	for i := range rs {
		r = &rs[i]
		if (r.X+r.W+1) < x || r.X > (x+w+1) {
			continue
		}
		if (r.Y+r.H+1) < y || r.Y > (y+h+1) {
			continue
		}
		return true
	}
	return false
}

func MakeRoom(count int32, gen *uint32) *[]Room {
	rs := make([]Room, 100)
	counter := int32(0)
        for i := int32(0); i < count; i++ {
		r1 := GenRand(gen)
		r2 := GenRand(gen)
		x := r1 % TileDim
		y := r2 % TileDim
		w := r1%MaxWid + MinWid
		h := r2%MaxWid + MinWid

		if x+w >= TileDim || y+h >= TileDim || x == 0 || y == 0 {
			continue
		}
		iscrash := CheckColl(x, y, w, h, rs[0:counter])
		if iscrash == false {
			rs[counter] = Room{x,y,w,h,counter}
			counter++
		}
		if counter == 99 {
			break
		}
	}
	x := rs[0:counter]
	return &x
}

func Room2Tiles(r *Room, ts *[]Tile) {
	x := r.X
	y := r.Y
	w := r.W
	h := r.H
	for xi := x; xi <= x+w; xi++ {
		for yi := y; yi <= y+h; yi++ {
			num := yi*TileDim + xi
			(*ts)[num].T = 1
		}
	}
}

func PrintLev(l *Lev) {
	for i, t := range l.ts {
		fmt.Printf("%v", t.T)
		if i%(TileDim) == 49 && i != 0 {
			fmt.Print("\n")
		}
	}
}

var vflag = flag.Int("v", 18, "Random Seed")

func main() {
	start := time.Now()
	flag.Parse()
	var v int = *vflag
	fmt.Printf("Random seed: %v\n", v)
	gen := ^uint32(v)
	ls := make([]Lev, 100)
	for i := 0; i < 100; i++ {
		rs := MakeRoom(50000, &gen)
		ts := make([]Tile, 2500)
		for ii := int32(0); ii < 2500; ii++ {
			ts[ii] = Tile{X: ii % TileDim, Y: ii / TileDim, T: 0}
		}
		for _, r := range *rs {
			Room2Tiles(&r, &ts)
		}
		ls[i] = Lev{rs: *rs, ts: ts}
	}
	templ := Lev{}
	for i := 0; i < 100; i++ {
		if len(ls[i].rs) > len(templ.rs) {
			templ = ls[i]
		}
	}
	PrintLev(&templ)
	end := time.Now()
	fmt.Printf("Time in ms: %d\n", (end.Sub(start) / 1000000))
}
