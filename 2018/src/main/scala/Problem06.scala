import scala.collection.mutable
import scala.io.Source

object Problem06 extends App {

  case class Point(x: Int, y: Int, index: Int) {
    def distance(otherX: Int, otherY: Int): Int = {
      Math.abs(x - otherX) + Math.abs(y - otherY)
    }
  }
  //val points = Source.fromResource("06-test.txt").getLines().map(_.split(", ").map(_.toInt)).zipWithIndex.map{ case (arr, index) => Point(arr(0), arr(1), index)}.toVector
  val points = Source.fromResource("06-input.txt").getLines().map(_.split(", ").map(_.toInt)).zipWithIndex.map{ case (arr, index) => Point(arr(0), arr(1), index)}.toVector

  //val max = 15
  val max = 500

  println(s"Min x : ${points.map(_.x).min} Max x : ${points.map(_.x).max}")
  println(s"Min y : ${points.map(_.y).min} Max y : ${points.map(_.y).max}")

  val grid = Array.fill(max, max)("")
//  for {
//    x <- 0 until max
//    y <- 0 until max
//  } yield {
//    val sortedPoints = points.sortBy(_.distance(x, y))
//    if (sortedPoints(0).distance(x, y) == sortedPoints(1).distance(x, y)) {
//      grid(x)(y) = "."
//    } else {
//      grid(x)(y) = sortedPoints.head.index.toString
//    }
//  }

  //grid.foreach(row => println(row.mkString))

//  val areas = grid.map(_.groupBy(identity).mapValues(_.length)).reduce { (map1, map2) =>
//    val merged = map1.toSeq ++ map2.toSeq
//    val grouped = merged.groupBy(_._1)
//    val cleaned = grouped.mapValues(_.map(_._2).toList)
//    cleaned.mapValues(_.sum)
//  }
//
//  val ignored = mutable.Set(".")
//
//  for { x <- 0 until max } yield { ignored.add(grid(0)(x)); ignored.add(grid(max-1)(x)) }
//  for { y <- 0 until max } yield { ignored.add(grid(y)(0)); ignored.add(grid(y)(max-1)) }
//
//  val solution = areas.filterKeys(key => !ignored.contains(key)).values.toVector.max
//  println(solution)
  for {
    x <- 0 until max
    y <- 0 until max
  } yield {
    val distances = points.map(_.distance(x, y))
    if (distances.sum < 10000) {
      grid(x)(y) = "#"
    }
  }

  println(grid.map(_.count(_ == "#")).sum)






}
