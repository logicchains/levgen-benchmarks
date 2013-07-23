import std.getopt;
import std.stdio;
import std.random;

const int TileDim=50;
const int MinWid=2;
const int MaxWid=8;


void main(string[] args) {
	int v;		
	getopt(
	args, "v", &v);
	writefln("The random seed is: %s",v);
	rndGen.seed(v);
	Lev[100] ls;
	int lenls=0;	
	for (auto i=0; i<100; i++) {
		Room[100] rs;
		int lenrs=0;
		int *Plenrs = &lenrs;
		auto ii =0 ;
		for (ii=0;ii<50000;ii++){
			MakeRoom(rs.ptr,Plenrs);
			if(lenrs==99){ 
				break;
			}
		}
		Tile[2500] ts;
		for (ii=0;ii<2500;ii++){
			ts[ii].X=ii%TileDim;
			ts[ii].Y=ii/TileDim;
			ts[ii].T=0;
		}
		for (ii=0;ii<lenrs;ii++){
			Room2Tiles(&(rs[ii]),ts.ptr);
		}
		Lev l;
		l.rs = rs;
		l.ts = ts;
		l.lenrs=lenrs;
		ls[lenls]=l;
		lenls++;				
	}
	Lev templ;
	templ.lenrs=0;
	for(auto i=0;i<100;i++){
		if(ls[i].lenrs>templ.lenrs) templ=ls[i];
	}
	PrintLev(&templ);    	
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

int CheckColl(int x,int y,int w,int h, Room* rs, int lenrs){
	int i=0;
	for(i=0;i<lenrs;i++){
		int rx = rs[i].X;
		int ry = rs[i].Y;
		int rw = rs[i].W;
		int rh = rs[i].H;
		bool RoomOkay;
		if((((rx + rw +1 ) < x) || ((rx > (x+w +1 ))))) { RoomOkay=1;} 
		else if((((ry + rh +1 ) < y) || ((ry > (y+h +1 ))))) {RoomOkay=1;}
		else {RoomOkay=0;}
		if(RoomOkay==0) return 1;
	}
	return 0;
}

void MakeRoom(Room* rs, int *lenrs){
	int x = uniform(0,TileDim);
	int y = uniform(0,TileDim);
	int w = uniform(MinWid,MaxWid);
	int h = uniform(MinWid,MaxWid);
	
	if(x+w>=TileDim || y+h>=TileDim || x==0 || y==0) return;
	int nocrash = CheckColl(x,y,w,h,rs,*lenrs);
	if (nocrash==0){
		Room r;
		r.X = x;
		r.Y = y;
		r.W = w;
		r.H = h;
		r.N = *lenrs;
		rs[*lenrs]=r;
		*lenrs = *lenrs+1;
	}
} 

void Room2Tiles(Room *r, Tile* ts){			
	int x=r.X;
	int y=r.Y;
	int w=r.W;
	int h=r.H;
	int xi;
	int yi;
	for(xi=x;xi<=x+w;xi++){
		for(yi=y;yi<=y+h;yi++){
			int num = yi*TileDim+xi;
			ts[num].T=1;	
		}
	}
} 

void PrintLev(Lev *l){
	int i =0;
	for(i=0;i<2500;i++){
		printf("%d", l.ts[i].T);
		if(i%(TileDim)==49 && i!=0)printf("\n");	
	}
}


