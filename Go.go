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
	X int
	Y int
	T int
}

type Room struct {
	X int
	Y int
	W int
	H int
	N int
}

type Lev struct {
	ts []Tile
	rs []Room
}

func GenRand(gen *uint32) int {
	*gen += *gen
	*gen ^= 1
	if int32(*gen) < 0 {
		*gen ^= 0x88888eef
	}
	a := *gen
	return int(a)
}

func CheckColl(x, y, w, h int, rs []Room) bool {
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

func MakeRoom(rs []Room, gen *uint32) []Room {
	x := GenRand(gen) % TileDim
	y := GenRand(gen) % TileDim
	w := GenRand(gen)%MaxWid + MinWid
	h := GenRand(gen)%MaxWid + MinWid

	if x+w >= TileDim || y+h >= TileDim || x == 0 || y == 0 {
		return rs
	}
	switch CheckColl(x, y, w, h, rs) {
	case false:
		rs = append(rs,
			Room{
				X: x,
				Y: y,
				W: w,
				H: h,
				N: len(rs),
			},
		)
	case true:
		rs = MakeRoom(rs, gen)
	}
	return rs
}

func Room2Tiles(r *Room, ts []Tile) {
	x := r.X
	y := r.Y
	w := r.W
	h := r.H
	for xi := x; xi <= x+w; xi++ {
		for yi := y; yi <= y+h; yi++ {
			num := yi*TileDim + xi
			ts[num].T = 1
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
	const NLEVELS = 100
	const NROOMS = 99
	const NTILES = 2500

	ls := make([]Lev, 0, 100)
	all_rooms := make([]Room, NROOMS*NLEVELS)
	all_tiles := make([]Tile, NTILES*NLEVELS)

	for i := 0; i < NLEVELS; i++ {

		rs := all_rooms[i*NROOMS : (i+1)*NROOMS][:0]
		for ii := 0; ii < NROOMS; ii++ {
			rs = MakeRoom(rs, &gen)
		}

		ts := all_tiles[i*NTILES : (i+1)*NTILES][:0]
		for ii := 0; ii < NTILES; ii++ {
			ts = append(ts, Tile{X: ii % TileDim, Y: ii / TileDim, T: 0})
		}
		for _, r := range rs {
			Room2Tiles(&r, ts)
		}
		ls = append(ls, Lev{rs: rs, ts: ts})
	}
	templ := Lev{}
	for i := 0; i < NLEVELS; i++ {
		if len(ls[i].rs) > len(templ.rs) {
			templ = ls[i]
		}
	}
	PrintLev(&templ)
	end := time.Now()
	fmt.Printf("Time in ms: %d\n", (end.Sub(start) / 1000000))
}
