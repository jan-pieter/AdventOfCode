import scala.io.Source

object Problem05 extends App:
//  val file = "05-test.txt"
  val file = "05-input.txt"
  val input = Source.fromResource(file).getLines().toVector
  val (rules, printed) = {
    val split = input.indexWhere(_.isEmpty)
    (
      input.take(split).map { case s"$first|$second" => (first, second)},
      input.drop(split+1).map(_.split(",").toVector)
    )
  }

  def correctOrder(rule: (String, String), input: Vector[String]): Boolean = {
    val firstPos = input.indexOf(rule._1)
    val secondPos = input.indexOf(rule._2)
    firstPos == -1 || secondPos == -1 || firstPos < secondPos
  }

  val result = printed
    .filter(pages => rules.forall(rule => correctOrder(rule, pages)))
    .map(pages => pages(pages.length/2).toInt)
    .sum
  println(result)

  def fixPages(pages: Vector[String], rule: (String, String)): Vector[String] = {
    val firstPos = pages.indexOf(rule._1)
    val secondPos = pages.indexOf(rule._2)
    if firstPos == -1 || secondPos == -1 || firstPos < secondPos then pages
    else {
      val (left, right) = pages.splitAt(secondPos)
      val (middle, rest) = right.splitAt(firstPos-secondPos)
      left ++ Vector(rule._1) ++ middle ++ rest.drop(1)
    }
  }

  val result2 = printed
    .filterNot(pages => rules.forall(rule => correctOrder(rule, pages)))
    .map { pages =>
      var fixed = pages
      while (rules.exists(rule => !correctOrder(rule, fixed))){
        fixed = rules.foldLeft(fixed)(fixPages)
      }
      fixed(fixed.length/2).toInt
    }
  println(result2.sum)
