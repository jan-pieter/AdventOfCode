import scala.collection.mutable
import scala.io.Source

object Problem21 extends App:
  val input = Source.fromResource("21-input.txt").getLines().toVector
  val layers = 2
  val inputWithoutS = input.map(_.replace("S", "."))
  val expandedInputCol = Vector.fill(1 + 2 * layers)(inputWithoutS).flatten
  val expandedInput = {
    val t = expandedInputCol.transpose
    Vector.fill(1 + 2 * layers)(t).flatten.transpose
  }
//  expandedInput.foreach(line => println(line.mkString))
  val startPos = input.zipWithIndex.map(t => t._2 -> t._1.indexOf('S')).find(_._2 >= 0).map(t => (t._1 + layers*input.length, t._2 + layers*input.head.length)).get
//  val plots = input.zipWithIndex.flatMap(t => t._1.zipWithIndex.filter(_._1 == '.').map(l => t._2 -> l._2))
  println(s"Start $startPos")
//  println(s"Plots ${plots.mkString(",")}")
  val steps = 50
  def distance(pos1: (Int, Int), pos2: (Int, Int)) = (pos1._1 - pos2._1).abs + (pos1._2 - pos2._2).abs

  val distances = Array.fill(expandedInput.length, expandedInput.head.length)(-1)
  distances(startPos._1)(startPos._2) = 0
  val queue = mutable.Queue[(Int, Int)](startPos)
  while (queue.nonEmpty) {
    val current = queue.dequeue()
    val neighbours = Vector((current._1-1, current._2), (current._1+1, current._2), (current._1, current._2-1), (current._1, current._2+1))
    neighbours
      .filter(n => n._1 >= 0 && n._2 >= 0 && n._1 < expandedInput.length && n._2 < expandedInput.head.length)
      .filter(n => distances(n._1)(n._2) == -1)
      .filter(n => expandedInput(n._1)(n._2) == '.')
      .foreach { n =>
        distances(n._1)(n._2) = distances(current._1)(current._2) + 1
        queue.addOne(n)
      }
  }
//  distances.foreach(l => println(l.map(d => if d == -1 then "  #" else f"$d%03d").mkString(" ")))
  val result = distances.map(_.count(distance => distance >= 0 && distance <= steps && distance % 2 == steps % 2)).sum
  println(result)

  val part2steps = 26501365L
  val half = input.length / 2
  val y1 = distances.map(_.count(distance => distance >= 0 && distance <= half && distance % 2 == part2steps % 2)).sum.toLong
  val y2 = distances.map(_.count(distance => distance >= 0 && distance <= (half + input.length) && distance % 2 == part2steps % 2)).sum.toLong
  val y3 = distances.map(_.count(distance => distance >= 0 && distance <= (half + input.length * 2) && distance % 2 == part2steps % 2)).sum.toLong
  println(s"$y1 $y2 $y3")

  val x1 = 0
  val x2 = 1
  val x3 = 2

  val a = (x1*(y3-y2)+x2*(y1-y3)+x3*(y2-y1))/((x1-x2)*(x1-x3)*(x2-x3))
  val b = ((y2-y1)/(x2-x1)) - (a * (x1+x2))
  val c = y1 - a*x1*x1 - b * x1
  println(s"$a $b $c")

  val x = (part2steps - half)/input.length
  def polyValue2(x: Long) = a * x * x + b * x + c
  println(s"${polyValue2(x)}")

  //Wrong 625914369279475
  //Wrong 625914369188799
