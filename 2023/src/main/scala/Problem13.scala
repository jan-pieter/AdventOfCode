import scala.io.Source

object Problem13 extends App:
  val input = Source.fromResource("13-input.txt").getLines().toVector.foldLeft(Vector(Vector.empty[String])) {
    case (state, "") => state.appended(Vector.empty[String])
    case (state, line) => state.updated(state.length-1, state(state.length-1).appended(line))
  }

//  println(input)

  def reflectsOnRow(grid: Vector[Vector[Char]]): Option[Int] = {
    grid.indices.drop(1).find{ mirror =>
      (1 to Math.min(mirror, grid.length-mirror)).forall(i => grid(mirror-i) == grid(mirror+i-1))
    }
  }

  val result = input.flatMap(grid => reflectsOnRow(grid.map(_.toVector)).map(_*100).orElse(reflectsOnRow(grid.transpose)))
//  println(result)
  println(result.sum)

  def reflectsOnRowSmudge(grid: Vector[Vector[Char]]): Option[Int] = {
    grid.indices.drop(1).find { mirror =>
      val smudges = (1 to Math.min(mirror, grid.length - mirror)).map { i =>
        val diff = grid(mirror - i).zipWithIndex.diff(grid(mirror + i - 1).zipWithIndex)
//        println(s"$mirror $i $diff")
        diff.length
      }
      smudges.sum == 1
    }
  }

  val result2 = input.flatMap(grid => reflectsOnRowSmudge(grid.map(_.toVector)).map(_ * 100).orElse(reflectsOnRowSmudge(grid.transpose)))
//  println(result2)
  println(result2.sum)
