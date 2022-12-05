import scala.io.Source

object Problem05 extends App:
  case class Move(count: Int, from: Int, to: Int)
  case class Solution(stacks: Vector[Vector[String]], moves: Vector[Move]) {
    def execute(atOnce: Boolean): Solution = Solution(moves.foldLeft(stacks){
      case (stacks, move) =>
        val toMove = stacks(move.from - 1).take(move.count)
        stacks
          .updated(move.from - 1, stacks(move.from - 1).drop(move.count))
          .updated(move.to - 1, (if (atOnce) toMove else toMove.reverse) ++ stacks(move.to - 1))
    }, Vector.empty)
    def topCrates: String = stacks.flatMap(_.headOption).mkString
  }

  val input = Source.fromResource("05-input.txt").getLines().toVector.foldLeft(Solution(Vector.fill(10)(Vector.empty), Vector.empty)) {
    case (solution, s"move $count from $from to $to") => solution.copy(moves = solution.moves :+ Move(count.toInt, from.toInt, to.toInt))
    case (solution, line) if line.contains("[") => line.grouped(4).map(_.trim).zipWithIndex.flatMap {
      case (s"[$crate]", index) => Some(index -> crate)
      case _ => None
    }.foldLeft(solution){
      case (solution, (index, crate)) => solution.copy(stacks = solution.stacks.updated(index, solution.stacks(index) :+ crate))
    }
    case (solution, _) => solution
  }

  val solution1 = input.execute(atOnce = false).topCrates
  println(solution1)

  val solution2 = input.execute(atOnce = true).topCrates
  println(solution2)
