import scala.io.Source

object Problem02 extends App:
//  val file = "02-test.txt"
  val file = "02-input.txt"
  val input = Source.fromResource(file).getLines().toVector
  val levels = input.map(_.split(" ").toVector.map(_.toInt))
  def isSafe(level: Vector[Int]): Boolean =
    (level.sliding(2).forall(pair => pair(1) - pair(0) > 0) || level.sliding(2).forall(pair => pair(1) - pair(0) < 0)) &&
    level.sliding(2).forall(pair => (pair(1) - pair(0)).abs >= 1 && (pair(1) - pair(0)).abs <= 3)
  println(levels.count(isSafe))

  def isSafe2(level: Vector[Int]): Boolean =
    isSafe(level) || level.indices.exists(i => isSafe(level.slice(0, i) ++ level.slice(i+1, level.length)))
  println(levels.count(isSafe2))