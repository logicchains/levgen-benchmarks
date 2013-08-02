package main

import (
  "flag"
	"fmt"
	"time"
	asm "goasm"
)

const (
	TileDim = 50
	MinWid  = 2
	MaxWid  = 8
)

type Tile struct {
	X uint32
	Y uint32
	T uint32
}

type Room struct {
	X uint32
	Y uint32
	W uint32
	H uint32
	N uint32
}

type Lev struct {
	ts []Tile
	rs []Room
}

func GenRand(gen *uint32) uint32 {
	seed := (*gen << 1) + 1
	if int32(seed) < 0 {
		seed ^= 0x88888eef
	}
	*gen = seed
	return uint32(seed)
}

func CheckColl(x, y, w, h uint32, rs []Room) bool {
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

func MakeRoom(count uint32, gen *uint32) *[]Room {
	rs := make([]Room, 100)
	counter := uint32(0)
        for i := uint32(0); i < count; i++ {
		r1,r2 := asm.Gen2Rands(gen)		
		//r1 := GenRand(gen)
		//r2 := GenRand(gen)
		x := r1 % TileDim
		y := r2 % TileDim
		if x*y == 0 {
			continue
		}
		w := r1 % MaxWid + MinWid
		h := r2 % MaxWid + MinWid
		if x+w >= TileDim || y+h >= TileDim {
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
		for ii := uint32(0); ii < 2500; ii++ {
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
