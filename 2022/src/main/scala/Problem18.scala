import scala.annotation.tailrec
import scala.io.Source

object Problem18 extends App:
  case class Cube(x: Int, y: Int, z: Int)
  val input = Source.fromResource("18-input.txt").getLines().toVector.map {
    case s"$x,$y,$z" => Cube(x.toInt, y.toInt, z.toInt)
  }

  val max = input.map(cube => Vector(cube.x, cube.y, cube.z).max).max

  val array = Array.fill(max+2, max+2, max+2)('.')

  input.foreach(cube => array(cube.x)(cube.y)(cube.z) = '#')

  def count(char: Char): Int = {
    (for {
      x <- array.indices
      y <- array.head.indices
      z <- array.head.head.indices
    } yield {
      if (array(x)(y)(z) == '#') {
        Vector(
          if (x == 0 || array(x - 1)(y)(z) == char) 1 else 0,
          if (array(x + 1)(y)(z) == char) 1 else 0,
          if (y == 0 || array(x)(y - 1)(z) == char) 1 else 0,
          if (array(x)(y + 1)(z) == char) 1 else 0,
          if (z == 0 || array(x)(y)(z - 1) == char) 1 else 0,
          if (array(x)(y)(z + 1) == char) 1 else 0
        ).sum
      } else 0
    }).sum
  }

  val solution1 = count('.')
  println(solution1)

  def paint(x: Int, y: Int, z: Int): Unit = {
    if (x >= 0 && x < array.length && y >= 0 && y < array.head.length && z >= 0 && z < array.head.head.length) {
      if (array(x)(y)(z) == '.') {
        array(x)(y)(z) = 'x'
        paint(x-1, y, z)
        paint(x+1, y, z)
        paint(x, y-1, z)
        paint(x, y+1, z)
        paint(x, y, z-1)
        paint(x, y, z+1)
      }
    }
  }
  paint(0,0,0)

  val solution2 = count('x')
  println(solution2)
