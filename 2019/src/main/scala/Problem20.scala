import scala.collection.mutable
import scala.io.Source

object Problem20 extends App {

  val world: Array[Array[Char]] = Source.fromResource("20-input.txt").getLines().map(_.toCharArray).toArray
  var width = world.map(_.length).max
  var height = world.length

  println(s"$width x $height")

  val portals: List[((Int, Int), (Int, Int), String)] = (for {
    x <- 0 until width
    y <- 0 until height
  } yield {
    if (y < height - 2 && world(y)(x).isLetter && world(y + 1)(x).isLetter && world(y + 2)(x) == '.') {
      Some((y + 1, x), (y+2, x), "" + world(y)(x) + world(y + 1)(x))
    } else if (y >= 2 && world(y - 2)(x) == '.' && world(y - 1)(x).isLetter && world(y)(x).isLetter) {
      Some((y - 1, x), (y-2, x), "" + world(y - 1)(x) + world(y)(x))
    } else if (x < world(y).length - 2 && world(y)(x).isLetter && world(y)(x + 1).isLetter && world(y)(x + 2) == '.') {
      Some((y, x + 1), (y,x+2), "" + world(y)(x) + world(y)(x + 1))
    } else if (x >= 2 && world(y)(x - 2) == '.' && world(y)(x - 1).isLetter && world(y)(x).isLetter) {
      Some((y, x - 1), (y, x-2), "" + world(y)(x - 1) + world(y)(x))
    } else {
      None
    }
  }).toList.flatten

  println(portals)
  
  val start = portals.find(_._3 == "AA").get._2
  val end = portals.find(_._3 == "ZZ").get._1
  
  println(s"Start: $start End: $end")

  def solve(start: (Int, Int)): Int = {
    val toVisit: mutable.Queue[((Int, Int), Int)] = mutable.Queue(start -> 0)
    val beenTo: mutable.Set[(Int,Int)] = mutable.Set(start)
    var shortest = Int.MaxValue
    def takeStep(elem: ((Int, Int), Int)): Unit = {
      val (newLocation, distance) = elem
      for {
        (x, y) <- List((-1,0),(1,0),(0,-1),(0,1))
      } yield {
        val afterStep = (newLocation._1+y, newLocation._2+x)
        if (afterStep._1 >= 0 && afterStep._2 >= 0 && afterStep._1 < world(0).length && afterStep._2 < world.length && !beenTo.contains(afterStep)) {
          beenTo.add(afterStep)
          world(afterStep._1)(afterStep._2) match {
            case '.' => toVisit.enqueue(afterStep -> (distance + 1))
            case '#' => //no-op
            case _ =>
              val portal = portals.find(p => p._1 == afterStep).get
              if (portal._3 != "AA") {
                if (portal._3 == "ZZ") {
                  shortest = distance
                } else {
                  val otherSide = portals.find(p => p._3 == portal._3 && p != portal).get
                  toVisit.enqueue(otherSide._2 -> (distance + 1))
                  beenTo.add(otherSide._1)
                }
              }
          }
        }
      }
    }
    while (toVisit.nonEmpty && shortest == Int.MaxValue)
      takeStep(toVisit.dequeue())
    shortest
  }
  println(s"Shortest: ${solve(start)}")


}
