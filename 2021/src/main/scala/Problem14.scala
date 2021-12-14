import scala.collection.mutable
import scala.io.Source

object Problem14 extends App {
  val input: Vector[String] = Source.fromResource("14-input.txt").getLines().toVector

  val start = input.head
  val rules = input.drop(2).map { s =>
    val parts = s.split(" -> ")
    parts(0) -> parts(1)
  }.toMap

  println(start)
  println(rules)

  def step(str: String): String ={
    str.sliding(2).map{ part =>
      rules.get(part).map { sub =>
        part(0) + sub
      }.getOrElse(part.dropRight(1))
    }.mkString + str.takeRight(1)
  }

  val solution1 = (1 to 10).foldLeft(start) {
    case (str, i) =>
      val result = step(str)
      //println(s"Step $i: $result")
      result
  }

  val histo: Vector[(Char, String)] = solution1.groupBy(identity).toVector.sortBy(_._2.length)
  val solution1Count = histo.last._2.length - histo.head._2.length
  println(s"Solution1: $solution1Count")

  val cache: mutable.Map[(String, Int), Map[Char, Long]] = mutable.Map.empty

  def merge(map1: Map[Char, Long], map2: Map[Char, Long]): Map[Char, Long] = {
    val merged = map1.toSeq ++ map2.toSeq
    val grouped = merged.groupBy(_._1)
    grouped.view.mapValues(_.map(_._2).sum).toMap
  }

//  val m1 = Map('a' -> 1L, 'b' -> 2L)
//  val m2 = Map('b' -> 2L, 'c' -> 3L)
//  println(merge(m1,m2))

  def findHisto(str: String, stepNr: Int): Map[Char, Long] = {
    if (stepNr == 0) {
      str.groupBy(identity).map {
        case (str, values) => str -> values.length.toLong
      }
    } else {
      cache.getOrElseUpdate(str -> stepNr, {
        val afterStep = step(str)
        //println(s"$str -> $afterStep")
        val s: Seq[(Char, Long)] = afterStep.drop(1).dropRight(1).map(c => c -> -1L)
        val t: Map[Char, Long] = s.groupBy(_._1).view.mapValues(_.map(_._2).sum).toMap
        step(str).sliding(2).map(substr => findHisto(substr, stepNr - 1)).foldLeft(t) {
          case (intermediate, newMap) => merge(intermediate, newMap)
        }
      })
    }
  }

  val result = findHisto(start, 40).toVector.sortBy(_._2)
  //println(s"REsult: $result")
  val solution2Count = result.last._2 - result.head._2
  println(s"Solution2: $solution2Count")
}
