import scala.collection.mutable
import scala.io.Source

object Problem06 extends App:
//  val file = "06-test.txt"
  val file = "06-input.txt"
  val input = Source.fromResource(file).getLines().toVector.map(_.toVector)

  val guardY = input.indexWhere(_.contains('^'))
  val guardX = input(guardY).indexOf('^')

  def walkGuard(input: Vector[Vector[Char]]): (Set[(Int, Int)], Boolean) =
    var guardDir = (-1, 0)
    var guardNext = (guardY + guardDir._1, guardX + guardDir._2)
    val visited = mutable.Set[(Int, Int)]((guardY, guardX))
    val visitedWithDir = mutable.Set[((Int, Int), (Int, Int))]((guardY, guardX) -> guardDir)
    while (guardNext._1 >= 0 && guardNext._1 < input.length && guardNext._2 >= 0 && guardNext._2 < input(guardNext._1).length && !visitedWithDir.contains(guardNext -> guardDir))  {
      if input(guardNext._1)(guardNext._2) == '#' then
        guardNext = guardDir match
          case (1, 0) => (guardNext._1 - 1, guardNext._2 - 1)
          case (0, 1) => (guardNext._1 + 1, guardNext._2 - 1)
          case (-1, 0) => (guardNext._1 + 1, guardNext._2 + 1)
          case (0, -1) => (guardNext._1 - 1, guardNext._2 + 1)
        guardDir = guardDir match
          case (1, 0) => (0, -1)
          case (0, 1) => (1, 0)
          case (-1, 0) => (0, 1)
          case (0, -1) => (-1, 0)
      else
        visited.add(guardNext)
        visitedWithDir.add(guardNext -> guardDir)
        guardNext = (guardNext._1 + guardDir._1, guardNext._2 + guardDir._2)
    }
    (visited.toSet, visitedWithDir.contains(guardNext -> guardDir))

  val result1 = walkGuard(input)
  println(result1._1.size)

  val result2 = result1._1.count(pos => walkGuard(input.updated(pos._1, input(pos._1).updated(pos._2, '#')))._2)
  println(result2)

