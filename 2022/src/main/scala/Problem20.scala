import scala.io.Source

object Problem20 extends App:
  case class Number(value: Int, hasMoved: Boolean)
  val input: Vector[Number] = Source.fromResource("20-input.txt").getLines().toVector.map(i => Number(i.toInt, false))
  println(input.mkString(" "))
  // Mix
  var mixed: Vector[Number] = input
  var next = mixed.indexWhere(!_.hasMoved)
  while(next >= 0) {
//    println(s"next $next with val ${mixed(next).value}")
    val element = mixed(next)
    val newRawIndex = next + (element.value % (mixed.length - 1))
    val newIndex = {
      if (newRawIndex > 0 && newRawIndex < mixed.length) newRawIndex
      else if (newRawIndex > 0) newRawIndex % (mixed.length - 1)
      else mixed.length + newRawIndex - 1
    }
//    val newIndex = (next + element.value) % (mixed.length - 1)
//    println(s"newIndex $newIndex")
    val withoutElement = mixed.patch(next, Nil, 1)
    if (newIndex <= next) {
      mixed = (mixed.take(newIndex) :+ element.copy(hasMoved = true)) ++ withoutElement.drop(newIndex)
    } else {
      mixed = (withoutElement.take(newIndex) :+ element.copy(hasMoved = true)) ++ withoutElement.drop(newIndex)
    }
//    println(mixed.map(_.value).mkString(" "))
    next = mixed.indexWhere(!_.hasMoved)
//    next = -1
  }

  val index0 = mixed.indexOf(Number(0, true))
  println(index0)
  val solution1 = List(1000, 2000, 3000).map(_ + index0).map(_ % mixed.length).map(mixed).map(_.value)
//  solution1.foreach(println(_))
  println(solution1.sum)


//  val solution2 = input.map(_.sum).sorted.takeRight(3).sum
//  println(solution2)
