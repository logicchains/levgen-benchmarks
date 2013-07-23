use std::rand::RngUtil;
use std::rand;
use std::os;
use std::int;

static TileDim:uint=50;
static MinWid:uint=2;
static MaxWid:uint=8;
	
fn main(){
	let args = os::args();
	let str = (args[1]);
	let mut v:int;
	match int::from_str(str){	
		Some(x) => v=x,
		None => v=18
	}	 
	println(fmt!("The random seed is: %?",v));
	let vstr  = int::to_str(v);
	let vbytes=vstr.as_bytes_with_null_consume();
	let mut rng= rand::IsaacRng::new_seeded(vbytes);
	
	let emptyr = Room {X:0,Y:0,W:0,H:0,N:0};
	let emptyt = Tile{X:0,Y:0,T:0};
	let emptyl = Lev{TS:[emptyt,..2500],RS:[emptyr,..100]};
	let mut ls : [Lev, ..100] = [emptyl,..100];
	let mut lslen = 0;
	let mut i:int = 0;
	while i<100{
		let mut rs : [Room,..100]= [emptyr,..100];
		let mut rslen = 0;
		let mut ii:uint = 0;
		while ii<50000{
			MakeRoom(& mut rs,&mut rng,&mut rslen);
			if rs.len()==99{ break}
			ii+=1;
		}
		let mut ts : [Tile,..2500]= [emptyt,..2500];
		ii=0;
		while ii<2500{
			let t = Tile{X:ii%TileDim,Y:ii/TileDim,T:0};
			ts[i]=t;
			ii+=1;
		}
		ii=0;
		while ii<rs.len(){
			Room2Tiles(&rs[ii],& mut ts);
			ii+=1;
		}
		let l = Lev{TS:ts,RS:rs};
		ls[lslen]=l;
		lslen+=1;
		i+=1;				
	}
	let BiggestLev = FindMostRooms(& mut ls);
	PrintLev(&ls[BiggestLev]);
}

struct Tile {
	X:uint,
	Y:uint,
	T:uint,
}

struct Room {
	X:uint, 
	Y:uint,
	W:uint, 
	H:uint, 
	N:uint 
}

struct Lev {
	TS:[Tile,..2500],
	RS:[Room,..100],
}

fn FindMostRooms(ls:&mut [Lev,..100]) -> int{
	let mut i=0;
	let mut max=0;
	let mut biggestLev=0;
	while i<100{
		if ls[i].RS.len()>max{ 
			max = ls[i].RS.len();
			biggestLev=i;
		}
		i+=1;
	}
	return biggestLev;
} 

fn MakeRoom(rs:&mut [Room,..100],rng:&mut rand::IsaacRng, rslen:&mut int) {
	let x = rng.gen_uint_range(0,TileDim);
	let y = rng.gen_uint_range(0,TileDim);
	let w = rng.gen_uint_range(MinWid,MaxWid);
	let h = rng.gen_uint_range(MinWid,MaxWid);
	if x+w>=TileDim || y+h>=TileDim || x==0 || y==0 { 
		return
	}
	let iscrash = CheckColl(x,y,w,h,rs);
	if iscrash==false{
		let r = Room {X:x,Y:y,W:w,H:h,N:rs.len()};
		rs[*rslen]=r;
		*rslen +=1;
	}
} 

fn CheckColl(x:uint,y:uint,w:uint,h : uint, rs: &mut [Room,..100]) -> bool{
	for rs.iter().advance |r|{
		let rx = r.X;
		let ry = r.Y;
		let rw = r.W;
		let rh = r.H;
		let mut RoomOkay =0;
		if((((rx + rw +1 ) < x) || ((rx > (x+w +1 ))))) { RoomOkay=1;} 
		else if((((ry + rh +1 ) < y) || ((ry > (y+h +1 ))))) {RoomOkay=1;}
		else {RoomOkay=0;}
		if(RoomOkay==0){ return true}
	}		
	return false
}

fn Room2Tiles(r :&Room, ts: &mut [Tile,..2500]){			
	let x=r.X;
	let y=r.Y;
	let w=r.W;
	let h=r.H;
	let mut xi = x;
	let mut yi = y;
	while(xi<=x+w){
		yi = y;
		while(yi<=y+h){
			let num= yi*TileDim+xi;
			ts[num].T=1;
			yi+=1;			
		}			
		xi+=1;		
	}
} 

fn PrintLev(l :&Lev){
	let mut i :uint = 0;
	while i < l.TS.len(){
		print(int::to_str(l.TS[i].T.to_int()));
		if i%(TileDim)==49 && i!=0 {
			print("\n");
		}
		i+=1;
	}
}
