import std.getopt;
import std.stdio;
import std.random;

immutable int TileDim=50;
immutable int MinWid=2;
immutable int MaxWid=8;

void main(string[] args) {
	int v;	
	getopt(args, "v", &v);
	writefln("The random seed is: %s",v);
	rndGen.seed(v);
	Lev[100] ls;
	foreach (ref l; ls) {
		Room[100] rs;
		int lenrs=0;
		foreach (_; 0 .. 50000) {
			lenrs = MakeRoom(rs.ptr,lenrs);
			if(lenrs==99){
				break;
			}
		}
		Tile[2500] ts;
		foreach (int ii, ref t; ts){
			t.X=ii%TileDim;
			t.Y=ii/TileDim;
			t.T=0;
		}
		foreach (r; rs[0 .. lenrs]) {
			Room2Tiles(r,ts);
		}
		l.rs = rs;
		l.ts = ts;
		l.lenrs=lenrs;
	}
	Lev templ;
	templ.lenrs=0;
	foreach (l; ls) {
		if(l.lenrs>templ.lenrs) templ=l;
	}
	PrintLev(templ);
}

struct Tile {
	int X;
	int Y;
	int T;
};

struct Room {
	int X;
	int Y;
	int W;
	int H;
	int N;
};

struct CD {
	int X;
	int Y;
	int L;
	int vert;
};

struct Lev{
	Tile[2500] ts;
	Room[100] rs;
	int lenrs;	
};

int CheckColl(int x,int y,int w,int h, Room* rs, const int lenrs){
	foreach (i; 0 .. lenrs){
		int rx = rs[i].X;
		int ry = rs[i].Y;
		int rw = rs[i].W;
		int rh = rs[i].H;
		bool RoomOkay;
		if (rx + rw + 1 < x || rx > x+w+1) {
			RoomOkay = true;
		} else if (ry + rh + 1 < y || ry > y+h+1) {
			RoomOkay = true;
		}
		if(!RoomOkay)
			return true;
	}
	return 0;
}

int MakeRoom(Room* rs, int lenrs){
	immutable int x = uniform(0,TileDim);
	immutable int y = uniform(0,TileDim);
	immutable int w = uniform(MinWid,MaxWid);
	immutable int h = uniform(MinWid,MaxWid);

	if(x+w>=TileDim || y+h>=TileDim || x==0 || y==0) return lenrs;
	int nocrash = CheckColl(x,y,w,h,rs,lenrs);
	if (nocrash==0){
		Room r;
		r.X = x;
		r.Y = y;
		r.W = w;
		r.H = h;
		r.N = lenrs;
		rs[lenrs]=r;
		return lenrs + 1;
	}

	return lenrs;
}

void Room2Tiles(const Room r, ref Tile[2500] ts){	
	immutable int x=r.X;
	immutable int y=r.Y;
	immutable int w=r.W;
	immutable int h=r.H;
	foreach (xi; x .. x+w+1) {
		foreach (yi; y .. y+h+1) {
			immutable int num = yi*TileDim+xi;
			ts[num].T=1;	
		}
	}
}

void PrintLev(Lev lev){
	foreach (i, l; lev.ts){
		printf("%d", l.T);
		if(i%(TileDim)==49 && i!=0)printf("\n");	
	}
}
