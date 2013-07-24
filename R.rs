use std::rand::{Rng, RngUtil};
use std::{rand, os, int, uint, vec};

static TileDim: uint = 50;
static MinWid: uint  = 2;
static MaxWid: uint  = 8;

fn main() {
    let args = os::args();
    let str = (args[1]);
    let v = int::from_str(str).get_or_default(18);
    println(fmt!("The random seed is: %?",v));

    let vstr = int::to_str(v);
    let vbytes = vstr.as_bytes_with_null_consume();
//    let mut rng = rand::IsaacRng::new_seeded(vbytes);

    let (a,b,c,d) = match vbytes {
        [] => fail!("no seed"),
        [a] => (a,a,a,a),
        [a,b] => (a,b,a,b),
        [a,b,c] => (a,b,c,a),
        [a,b,c,d, .. _] => (a,b,c,d)
    };
    let mut rng = rand::XorShiftRng::new_seeded(a as u32,b as u32,c as u32,d as u32);

    let ls: ~[Lev] = do vec::from_fn(100) |_| {
        let rs = rooms(&mut rng, 99);
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

fn rooms<R: Rng>(rng: &mut R, n: uint) -> ~[Room] {
    let mut rooms = vec::with_capacity(n);
    for 50000.times {
        let x = rng.gen_uint_range(0, TileDim);
        let y = rng.gen_uint_range(0, TileDim);
        let w = rng.gen_uint_range(MinWid, MaxWid);
        let h = rng.gen_uint_range(MinWid, MaxWid);
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
