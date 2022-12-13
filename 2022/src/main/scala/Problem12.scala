import scala.collection.mutable
import scala.io.Source

object Problem12 extends App:
  val input = Source.fromResource("12-input.txt").getLines().toVector

  def adjusted(c: Char) = c match {
    case 'E' => 'z'
    case 'S' => 'a'
    case c => c
  }
  case class Point(x: Int, y: Int) {
    def offGrid: Boolean            = x < 0 || y < 0 || x >= input.head.length || y >= input.length
    def reachable(c: Char): Boolean = adjusted(value) <= adjusted(c) + 1
    def value: Char = input(y)(x)
    def neighbours: Set[Point] = Set(Point(x - 1, y), Point(x + 1, y), Point(x, y - 1), Point(x, y + 1)).filterNot(_.offGrid).filter(_.reachable(value))
    def neighbours2: Set[Point] = Set(Point(x - 1, y), Point(x + 1, y), Point(x, y - 1), Point(x, y + 1)).filterNot(_.offGrid).filter(n => reachable(n.value))
  }

  val start: Point = {
    val y = input.indexWhere(_.contains('S'))
    val x = input(y).indexOf('S')
    Point(x, y)
  }
  println(s"Start: $start")
  val end: Point = {
    val y = input.indexWhere(_.contains('E'))
    val x = input(y).indexOf('E')
    Point(x, y)
  }
  println(s"End: $end")

  val queue: mutable.Queue[Point] = mutable.Queue(start)
  val visited: mutable.Set[Point] = mutable.Set(start)
  val distance: mutable.Map[Point, Long] = mutable.Map(start -> 0)

  while (queue.nonEmpty) {
    val p = queue.dequeue()
    if (p == end) {
      queue.empty
    } else {
      p.neighbours.diff(visited).foreach{ n =>
        visited.addOne(n)
        distance.put(n, distance(p) + 1)
        queue.enqueue(n)
      }
    }
  }

  val solution1 = distance(end)
  println(solution1)

  val queue2: mutable.Queue[Point] = mutable.Queue(end)
  val visited2: mutable.Set[Point] = mutable.Set(end)
  val distance2: mutable.Map[Point, Long] = mutable.Map(end -> 0)

  while (queue2.nonEmpty) {
    val p = queue2.dequeue()
//    println(s"At $p")
    p.neighbours2.diff(visited2).foreach { n =>
      visited2.addOne(n)
      distance2.put(n, distance2(p) + 1)
      queue2.enqueue(n)
//      println(s"Enqueued $n")
//      if(n.value == 'a') println("YES")
    }
  }

  val minA = (for {
    x <- input.head.indices
    y <- input.indices
  } yield Point(x, y)).filter(p => adjusted(p.value) == 'a').minBy(p => distance2.getOrElse(p, Long.MaxValue))
  val solution2 = distance2(minA)
  println(solution2)
