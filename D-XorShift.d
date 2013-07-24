// Compile with:
//   ldmd2 -O -release -inline -noboundscheck bench.d
//   Then if you want strip the binary.

import std.stdio, std.random, std.getopt;

enum uint tileDim = 50,
          minWid = 2,
          maxWid = 8;

struct Tile { uint x, y, t; }
struct Room { uint x, y, w, h, n; }

struct Lev {
    Tile[2_500] ts;
    Room[100] rs;
    uint lenRs;
}

bool checkColl(size_t N)(in uint x, in uint y, in uint w, in uint h,
                         in ref Room[N] rs)
pure nothrow {
    foreach (immutable /*ref*/ r; rs) {
        bool roomOK = false;
        if ((r.x + r.w + 1) < x || r.x > (x + w + 1))
            roomOK = true;
        else if ((r.y + r.h + 1) < y || r.y > (y + h + 1))
            roomOK = true;
        else
            roomOK = false;
        if (!roomOK)
            return true;
    }

    return false;
}

void makeRoom(size_t N)(ref Room[N] rs, ref uint lenRs, ref Xorshift rng) {
    immutable x = uniform(0, tileDim, rng);
    immutable y = uniform(0, tileDim, rng);
    immutable w = uniform(minWid, maxWid, rng);
    immutable h = uniform(minWid, maxWid, rng);

    if (x + w >= tileDim || y + h >= tileDim || x == 0 || y == 0)
        return;
    if (!checkColl(x, y, w, h, rs)) {
        rs[lenRs] = Room(x, y, w, h, lenRs);
        lenRs++;
    }
}

void room2Tiles(size_t N)(in ref Room r, ref Tile[N] ts) pure nothrow {
    foreach (immutable xi; r.x .. r.x + r.w + 1)
        foreach (immutable yi; r.y .. r.y + r.h + 1)
            ts[yi * tileDim + xi].t = 1;
}

void printLev(in ref Lev l) nothrow {
    foreach (immutable size_t i, const ref tsi; l.ts) {
        printf("%d", tsi.t);
        if (i % tileDim == (tileDim - 1) && i != 0)
            putchar('\n');
    }
}

void main(string[] args) {
    enum size_t nLoops = 50_000;

    if (args.length <= 1)
        return writeln("Integer seed argument required.");
    uint v;
    getopt(args, "v", &v);

    writeln("The random seed is: ", v);
    auto rng = Xorshift(v);
    Lev[Lev.rs.length] ls = void;

    uint lenLs = 0;
    foreach (immutable i; 0 .. Lev.rs.length) {
        Room[Lev.rs.length] rs;
        uint lenRs = 0;
        foreach (immutable _; 0 .. nLoops) {
            makeRoom(rs, lenRs, rng);
            if (lenRs == (Lev.rs.length - 1))
                break;
        }

        Tile[Lev.ts.length] ts = void;
        foreach (immutable uint j, ref tsj; ts)
            tsj = Tile(j % tileDim, j / tileDim, 0);

        foreach (immutable uint j; 0 .. lenRs)
            room2Tiles(rs[j], ts);

        ls[lenLs] = Lev(ts, rs, lenRs);
        lenLs++;
    }

    Lev templ = void;
    templ.lenRs = 0;
    foreach (const ref lsi; ls)
        if (lsi.lenRs > templ.lenRs)
            templ = lsi;

    templ.printLev;
}
