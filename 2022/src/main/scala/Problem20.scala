import scala.io.Source

object Problem20 extends App:
  case class Number(value: Long, originalIndex: Int)
  val input: Vector[Number] = Source.fromResource("20-input.txt").getLines().toVector.zipWithIndex.map((i, index) => Number(i.toLong, index))
  println(input.mkString(" "))
  def mix(prev: Vector[Number]): Vector[Number] = {
    // Mix
    var mixed: Vector[Number] = prev
    input.indices.foreach { index =>
//      val next = mixed.indexOf(Number(input(index).value, index))
      val next = mixed.indexWhere(_.originalIndex == index)
      //    println(s"next $next with val ${mixed(next).value}")
      val element = mixed(next)
      val newRawIndex = next + (element.value % (mixed.length - 1)).toInt
      val newIndex = {
        if (newRawIndex > 0 && newRawIndex < mixed.length) newRawIndex
        else if (newRawIndex > 0) newRawIndex % (mixed.length - 1)
        else mixed.length + newRawIndex - 1
      }
      //    val newIndex = (next + element.value) % (mixed.length - 1)
      //    println(s"newIndex $newIndex")
      val withoutElement = mixed.patch(next, Nil, 1)
      if (newIndex <= next) {
        mixed = (mixed.take(newIndex) :+ element) ++ withoutElement.drop(newIndex)
      } else {
        mixed = (withoutElement.take(newIndex) :+ element) ++ withoutElement.drop(newIndex)
      }
      //    println(mixed.map(_.value).mkString(" "))
    }
    mixed
  }

  def printSolution(mixed: Vector[Number]): Unit = {
    val index0 = mixed.indexWhere(_.value == 0)
    println(index0)
    val solution = List(1000, 2000, 3000).map(_ + index0).map(_ % mixed.length).map(mixed).map(_.value)
    println(solution.sum)
  }

  val mixed = mix(input)
  printSolution(mixed)

  val mixed2 = (0 until 10).foldLeft(input.map(number => number.copy(number.value * 811589153)))((prev, _) => mix(prev))
  printSolution(mixed2)
