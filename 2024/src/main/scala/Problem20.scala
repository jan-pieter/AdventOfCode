import scala.collection.mutable
import scala.io.Source

object Problem20 extends App:
//  val file = "20-test.txt"
  val file = "20-input.txt"
  val input = Source.fromResource(file).getLines().toVector.map(_.toVector)

  var currentY = input.indexWhere(_.contains('S'))
  var currentX = input(currentY).indexOf('S')
  var currentDistance = 0
  val distance = mutable.Map.empty[(Int, Int), Int]
  distance.addOne((currentY, currentX) -> currentDistance)
  while input(currentY)(currentX) != 'E' do
    val next = Vector((currentY - 1, currentX), (currentY + 1, currentX), (currentY, currentX - 1), (currentY, currentX + 1)).filter { case (y, x) =>
      input(y)(x) != '#' && !distance.contains((y, x))
    }
    if next.length != 1 then println(s"ERROR at $currentY $currentX: $next")
    val nextStep = next.head
    currentDistance = currentDistance + 1
    distance(nextStep) = currentDistance
    currentX = nextStep._2
    currentY = nextStep._1

  println("Distance: " + currentDistance)

  val result = distance.flatMap{ (k, d) =>
    val y = k._1
    val x = k._2
    Vector((-1, 0), (1, 0), (0, -1), (0, 1)).flatMap { case (dy, dx) =>
      if input(y+dy)(x+dx) == '#' then
        val (ny, nx) = (y + 2*dy, x + 2*dx)
        if ny < 0 || ny >= input.length || nx < 0 || nx >= input(ny).length then None
        else if distance.contains((ny, nx)) && distance((ny,nx)) < distance((y,x))+2 then Some(distance((y,x)) - (distance((ny,nx))+2))
        else None
      else None
    }
  }.toVector.groupBy(identity).map((k, v) => (k, v.length)).toVector.sortBy(_._1)
//  println(result)
  println(result.filter(_._1 >= 100).map(_._2).sum)

  val result2 = distance.toVector.flatMap { (k, d) =>
    val y = k._1
    val x = k._2
    val reachable = mutable.Map.empty[(Int, Int), Int]
    val toConsider = mutable.Queue.empty[((Int, Int), Int)]
    toConsider.enqueue((k, 0))
    reachable.addOne(k -> 0)
    while (toConsider.nonEmpty && toConsider.head._2 < 20) {
      val ((cy, cx), cd) = toConsider.dequeue()
      Vector((-1, 0), (1, 0), (0, -1), (0, 1)).foreach { case (dy, dx) =>
        val (ny, nx) = (cy + dy, cx + dx)
        if ny < 0 || ny >= input.length || nx < 0 || nx >= input(ny).length then ()
        else if reachable.contains((ny, nx)) then ()
        else
          reachable.addOne((ny, nx) -> (cd + 1))
          toConsider.enqueue(((ny, nx), cd + 1))
      }
    }
    reachable.toVector
      .filter((k,v) => (input(k._1)(k._2) == '.' || input(k._1)(k._2) == 'E') && (distance((y,x)) + v) < distance((k._1,k._2)))
      .map((k, v) => distance((k._1,k._2)) - (distance((y,x))+v))
  }.groupBy(identity).map((k, v) => (k, v.length)).toVector.sortBy(_._1)
  println(result2.filter(_._1 >= 100).map(_._2).sum)
