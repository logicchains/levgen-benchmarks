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

var seed uint32

func GenRand() uint32 {
	seed <<= 1
        sext := uint32(int32(seed)>>31) & 0x88888eef
	seed ^= sext ^ 1
	return seed
}

func CheckColl(x, y, w, h uint32, rs []Room) bool {
	var r *Room
	x_m1, xw_p1 := x-1, x+w+1
	y_m1, yh_p1 := y-1, y+h+1
	for i := range rs {
		r = &rs[i]
		switch{
		case r.X > xw_p1:
			continue
		case r.Y > yh_p1 :
			continue
		case (r.X + r.W) < x_m1:
			continue
		case (r.Y + r.H) < y_m1:
			continue
		}
		return true
	}
	return false
}

func MakeRoom(count uint32) *[]Room {
	rs := make([]Room, 100)
	counter := uint32(0)
        for i := uint32(0); i < count; i++ {
		r1 := GenRand()
		r2 := GenRand()
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
	seed = ^uint32(v)
	ls := make([]Lev, 100)
	for i := 0; i < 100; i++ {
		rs := MakeRoom(50000)
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
