import scala.collection.mutable
import scala.io.Source

object Problem03 extends App:
//  val file = "03-test.txt"
  val file = "03-input.txt"
  val input = Source.fromResource(file).getLines().toVector.map(_.toVector.map(_.asDigit))
  //println(input)

  def maxJolts2(j: Vector[Int]): Int = {
    if j.length == 2 then j.head*10 + j(1) else
      Math.max(maxJolts2(j.drop(1)), j.head*10 + j.drop(1).max)
  }

  println(input.map(maxJolts2).sum)

  def maxJoltsNCached(j: Vector[Int], n: Int): Long = {
    val cache: mutable.Map[(Vector[Int], Int), Long] = mutable.Map.empty

    def maxJoltsN(j: Vector[Int], n: Int): Long = {
      cache.getOrElseUpdate(j -> n, {
        if n == 0 then 0L else if j.length == n then j.map(_.toString).mkString("").toLong else
          Math.max(maxJoltsN(j.drop(1), n), j.head.toLong * Math.pow(10, n - 1).toLong + maxJoltsN(j.drop(1), n - 1))
      })
    }

    maxJoltsN(j, n)
  }

//  println(maxJoltsN(input.head, 12))
  println(input.map(maxJoltsNCached(_, 12)).sum)
