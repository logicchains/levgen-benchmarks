package main

import "fmt"
import "flag"
import "math/rand"

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

func CheckColl(x, y, w, h int, rs []Room) bool {
	for _, r := range rs {
		RoomOkay := true
		if ((r.X + r.W + 1) < x) || (r.X > (x + w + 1)) {
			RoomOkay = true
		} else if ((r.Y + r.H + 1) < y) || (r.Y > (y + h + 1)) {
			RoomOkay = true
		} else {
			RoomOkay = false
		}
		if RoomOkay == false {
			return true
		}
	}
	return false
}

func MakeRoom(rs *[]Room) {
	x := rng.Intn(TileDim)
	y := rng.Intn(TileDim)
	w := rng.Intn(MaxWid) + MinWid
	h := rng.Intn(MaxWid) + MinWid

	if x+w >= TileDim || y+h >= TileDim || x == 0 || y == 0 {
		return
	}
	iscrash := CheckColl(x, y, w, h, *rs)
	if iscrash == false {
		var r Room
		r.X = x
		r.Y = y
		r.W = w
		r.H = h
		r.N = len(*rs)
		*rs = append(*rs, r)
	}
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
var rng *rand.Rand

func main() {
	flag.Parse()
	var v int = *vflag
	fmt.Printf("Random seed: %v\n", v)
	rng = rand.New(rand.NewSource(int64(v)))
	ls := make([]Lev, 0, 100)
	for i := 0; i < 100; i++ {
		rs := make([]Room, 0, 100)
		for ii := 0; ii < 50000; ii++ {
			MakeRoom(&rs)
			if len(rs) == 99 {
				break
			}
		}
		ts := make([]Tile, 0, 2500)
		for ii := 0; ii < 2500; ii++ {
			t := Tile{X: ii % TileDim, Y: ii / TileDim, T: 0}
			ts = append(ts, t)
		}
		for _, r := range rs {
			Room2Tiles(&r, &ts)
		}
		var l Lev
		l.rs = rs
		l.ts = ts
		ls = append(ls, l)
	}
	templ := Lev{}
	for i := 0; i < 100; i++ {
		if len(ls[i].rs) > len(templ.rs) {
			templ = ls[i]
		}
	}
	PrintLev(&templ)
}
