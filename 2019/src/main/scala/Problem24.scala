import scala.collection.mutable
import scala.io.Source

object Problem24 extends App {

  val start = Source.fromResource("24-input.txt").getLines().toVector
  val width = 5
  val height = 5
  
  def neighbours(world: Vector[String], x: Int, y: Int): String = {
    (for {
      (xDiff,yDiff) <- List((-1, 0), (1, 0), (0,-1), (0, 1))
      newX = x + xDiff
      newY = y + yDiff
      if (newX >= 0 && newX < width)
      if (newY >=0 && newY < height)
    } yield world(newY)(newX)).mkString
  }
  
  def cellStep(world: Vector[String], x: Int, y: Int): Char = {
    world(y)(x) match {
      case '#' if neighbours(world, x, y).count(_ == '#') == 1 => '#'
      case '#' => '.'
      case '.' if neighbours(world, x, y).count(_ == '#') == 1 || neighbours(world, x, y).count(_ == '#') == 2 => '#'
      case '.' => '.'
    }
  }
  
  def step(world: Vector[String]): Vector[String] = {
    world.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (_, x) =>
        cellStep(world, x, y)
      }.mkString
    }
  }
  
  val seen: mutable.Set[String] = mutable.Set.empty
  var state = start
  while(!seen.contains(state.mkString)) {
    seen.add(state.mkString)
    state = step(state)
  }
  
  def rating(world: Vector[String]): Long = {
    world.mkString.zipWithIndex.map {
      case ('#', index) => Math.pow(2, index).toLong
      case _ => 0L
    }.sum
  }
  
  println("World:")
  state.foreach(println)
  println(s"Rating: ${rating(state)}")

}
