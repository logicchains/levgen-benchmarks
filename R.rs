use std::{os, int, uint, vec};

static TileDim: uint = 50;
static MinWid: uint  = 2;
static MaxWid: uint  = 8;

fn main() {
    let args = os::args();
    let str = (args[1]);
    let v = int::from_str(str).get_or_default(18);
    println(fmt!("The random seed is: %?",v));

    let mut prng = v.to_u32();

    let ls: ~[Lev] = do vec::from_fn(100) |_| {
        let rs = rooms(99,&mut prng);
        let mut ts: ~[Tile] = do vec::from_fn(TileDim * TileDim) |ii| {
            Tile {
                x: ii % TileDim,
                y: ii / TileDim,
                t: false
            }
        };

        for rs.iter().advance |r| {
            room_to_tiles(r, &mut ts);
        }
        Lev { tiles: ts, rooms: rs }
    };
    let biggest_lev = find_most_rooms(ls);
    print_lev(biggest_lev);
}

struct Tile {
    x: uint,
    y: uint,
    t: bool
}

struct Room {
    x: uint,
    y: uint,
    w: uint,
    h: uint,
    n: uint
}

struct Lev {
    tiles: ~[Tile],
    rooms: ~[Room],
}

fn find_most_rooms<'a>(ls: &'a [Lev]) -> &'a Lev {
    do ls.iter().max_by |lev| {
        lev.rooms.len()
    }.expect("oops, no levels")
}

fn rooms(n: uint,gen:&mut u32) -> ~[Room] {
    let mut rooms = vec::with_capacity(n);
    for 50000.times {
	let r1 = GenRand(gen);
	let r2 = GenRand(gen);
	let x = r1 % TileDim;
	let y = r2 % TileDim;
	let w = r1 % MaxWid+MinWid;
	let h = r2 % MaxWid+MinWid;
        if x + w < TileDim &&
           y + h < TileDim &&
           x != 0 &&
           y != 0 &&
           not_crash(x, y, w, h, rooms) {
            let r = Room { x: x, y: y, w: w, h: h, n: rooms.len() };
            rooms.push(r);
            if rooms.len() == n { break }
        }
    }
    rooms
}

fn not_crash(new_x: uint, new_y: uint, new_w: uint, new_h: uint, rs: &[Room]) -> bool {
    do rs.iter().all |r| {
        let Room { x, y, w, h, _ } = *r;

        ((x + w + 1) < new_x ||
         x > (new_x + new_w + 1) ||
         (y + h + 1) < new_y ||
         y > (new_y + new_h + 1))
    }
}

fn room_to_tiles(r: &Room, ts: &mut ~[Tile]) {
    let Room { x, y, w, h, _ } = *r;

    for uint::range(y, y + h + 1) |yi| {
        for uint::range(x, x + w + 1) |xi| {
            let num = yi * TileDim + xi;
            ts[num].t = true;
        }
    }
}

fn print_lev(l: &Lev) {
    for l.tiles.iter().enumerate().advance |(i, tile)| {
        print(if tile.t {"1"} else {"0"});
        if i % TileDim == 49 {
            print("\n");
        }
    }
}

fn GenRand(gen:&mut u32) ->uint { 
    *gen += *gen;
    *gen ^= 1;
	if (*gen).to_i32() < 0 {
        *gen ^= 0x88888eef;
    }
	return (*gen).to_uint();
}
