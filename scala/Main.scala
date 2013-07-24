import scala.util.Random

case class Tile(X: Int, Y: Int, var T: Int)

case class Room(X: Int, Y: Int, W: Int, H: Int, N: Int)

case class CD(X: Int, Y: Int, L: Int, vert: Int)

case class Lev(rs: Array[Room], ts: Array[Tile])

object Main {
  val TileDim = 50
  val MinWid = 2
  val MaxWid = 8

  def main(args: Array[String]) {
    println(s"The bench took ${time {

      val v = args(0).toLong
      println(s"The random seed is: $v")
      Random.setSeed(v)

      val ls = for(l <- 0 until 100) yield {
        val rs = rooms(Stream.empty).take(99).toArray
        val ts = new Array[Tile](2500)
        for(i <- 0 until 2500) {
          ts(i) = Tile(i % TileDim, i/TileDim, 0)
        }
        for(r <- rs) room2Tiles(r, ts)
        Lev(rs,ts)
      }
      printLev(ls sortBy(_.rs.length) head)

    }} ms")
  }

  def rooms(start: Stream[Room]): Stream[Room] = {
    val x = makeRoom(start)
    x #:: rooms(x #:: start)
  }

  def time(fn: => Unit) = {
    val start = System.currentTimeMillis()
    fn
    System.currentTimeMillis() - start
  }


  def makeRoom(rooms: Stream[Room]): Room = { //recursive, so result type must be specified
    val x = math.abs(Random.nextInt%TileDim)
    val y = math.abs(Random.nextInt%TileDim)
    val w = math.abs(Random.nextInt%MaxWid+MinWid)
    val h = math.abs(Random.nextInt%MaxWid+MinWid)
    if(x+w>=TileDim || y+h>=TileDim || x==0 || y==0)
      makeRoom(rooms)
    else if(!checkColl(x,y,w,h,rooms)) {
        Room(x,y,w,h,rooms.length)
    }
    else makeRoom(rooms)
  }

  def checkColl(x: Int,y: Int,w: Int,h: Int, rs: Stream[Room]): Boolean = {
    for (room <- rs) {
      if(!(room.X + room.W + 1 < x || room.X > x+w+1 || room.Y + room.H + 1 < y || room.Y > y+h+1))
        return true
    }
    false
  }

  def room2Tiles(r: Room, ts: Array[Tile]) = {
    val x=r.X
    val y=r.Y
    val w=r.W
    val h=r.H
    for (xi <- x until x+w+1; yi <- y until y+h+1) {
      val num = yi*TileDim+xi
      ts(num).T=1
    }
  }

  def printLev(lev: Lev){
    print((for (i <- 0 until 2500) yield
      lev.ts(i).T.toString + (if(i % TileDim == 49 && i != 0) "\n" else "")
    ).mkString)
  }
}

