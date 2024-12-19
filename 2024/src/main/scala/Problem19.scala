import scala.collection.mutable
import scala.io.Source

object Problem19 extends App:
//  val file = "19-test.txt"
  val file = "19-input.txt"
  val input = Source.fromResource(file).getLines().toVector
  val towels = input.head.split(", ").toVector
  val designs = input.drop(2)

  def solutionExists(design: String): Boolean =
    towels.filter(towel => towel.length <= design.length && design.startsWith(towel)).exists { towel =>
      design == towel || solutionExists(design.drop(towel.length))
    }

  val result = designs.map(solutionExists).count(identity)
  println(result)

  val cache = mutable.Map.empty[String, Long]

  def solutions(design: String): Long =
    cache.getOrElseUpdate(design, {
      towels.filter(towel => towel.length <= design.length && design.endsWith(towel)).map { towel =>
        if design == towel then 1L else solutions(design.dropRight(towel.length))
      }.sum
    })

  val result2 = designs.map(solutions)
  println(result2.sum)
