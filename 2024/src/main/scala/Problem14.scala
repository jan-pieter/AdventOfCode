import scala.io.Source

object Problem14 extends App:
//  val file = "14-test.txt"
  val file = "14-input.txt"
  val input = Source.fromResource(file).getLines().toVector
  val width = 101
  val height = 103
//  val width = 11
//  val height = 7
  case class Robot(x: Int, y: Int, vx: Int, vy: Int) {
    def positionAfter(steps: Long) = Robot(
      ((((x + vx*steps) % width) + width) % width).toInt,
      ((((y + vy*steps) % height) + height) % height).toInt,
      vx,
      vy
    )
    def toQuadrant: (Int, Int, Int, Int) =
      if (x < width/2 && y < height/2) (1, 0, 0, 0)
      else if ((x > width/2 || (width % 2 == 0 && x == width/2)) && y < height/2) (0, 1, 0, 0)
      else if (x < width/2 && (y > height/2 || (height % 2 == 0 && y == height/2))) (0, 0, 1, 0)
      else if ((x > width/2 || (width % 2 == 0 && x == width/2)) && (y > height/2 || (height % 2 == 0 && y == height/2))) (0, 0, 0, 1)
      else (0, 0, 0, 0)
  }

  val robots = input.map {
    case s"p=$x,$y v=$vx,$vy" => Robot(x.toInt, y.toInt, vx.toInt, vy.toInt)
  }

  val after100 = robots.map(_.positionAfter(100))
  val quadrants = after100.map(_.toQuadrant).foldLeft((0, 0, 0, 0)) {
    case ((q1, q2, q3, q4), (q1_, q2_, q3_, q4_)) => (q1+q1_, q2+q2_, q3+q3_, q4+q4_)
  }
  val answer = quadrants._1 * quadrants._2 * quadrants._3 * quadrants._4
  println(answer)

  def robotsToGrid(robots: Vector[Robot]) =
    val grid = Array.fill(height, width)('.')
    robots.foreach { robot =>
      grid(robot.y)(robot.x) = '#'
    }
    grid

  def printGrid(grid: Array[Array[Char]]) =
    grid.foreach { row =>
      println(row.mkString)
    }

  def robotNeighbours(robots: Vector[Robot]): Int =
    robots.map(robot =>
      robots.count(other => (robot.x - other.x).abs <= 1 && (robot.y - other.y).abs <= 1) - 1
    ).sum

  val result = (1 to width*height)
    .toVector
    .map(steps => steps -> robots.map(_.positionAfter(steps)))
    .maxBy((steps, robots) => robotNeighbours(robots))
  println(result._1)
  printGrid(robotsToGrid(result._2))
