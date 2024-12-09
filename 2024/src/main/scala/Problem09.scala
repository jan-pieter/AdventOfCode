import scala.io.Source

object Problem09 extends App:
//  val file = "09-test.txt"
  val file = "09-input.txt"
  val input = Source.fromResource(file).getLines().toVector.head.toVector
  val expanded = input.zipWithIndex.foldLeft(Vector.empty[Int]){
    case (acc, (c, i)) if i % 2 == 0 => acc ++ Vector.fill(c.asDigit)(i / 2)
    case (acc, (c, i)) if i % 2 == 1 => acc ++ Vector.fill(c.asDigit)(-1)
  }
  var result1 = expanded
  var i = 0
  var j = expanded.length-1
  while (i < j) {
    if (result1(i) >= 0) then i = i+1
    else if result1(j) < 0 then j = j-1
    else
      val origI = result1(i)
      result1 = result1.updated(i, result1(j)).updated(j, origI)
  }
  val answer1 = result1.zipWithIndex.foldLeft(0L) { case (acc, (c, i)) =>
    if c == -1 then acc else acc + i * c
  }
  println(answer1)
//  println(result1.mkString("").replace("-1", "."))

  var filesLeft = input.zipWithIndex.filter((c, i) => i % 2 == 0).map((c, i) => (c.asDigit, i))
  val result2 = input.zipWithIndex.foldLeft(Vector.empty[Int]){
    case (acc, (c, i)) if i % 2 == 1 || filesLeft.forall((d, j) => j != i) =>
      @annotation.tailrec
      def fillWithLastEligible(acc: Vector[Int], toFill: Int): Vector[Int] = {
        val rightEligible = filesLeft.findLast((d, j) => d <= toFill && i < j)
        rightEligible match {
          case None => acc ++ Vector.fill(toFill)(-1)
          case Some((d, j)) =>
            filesLeft = filesLeft.patch(filesLeft.lastIndexOf((d, j)), Nil, 1)
            fillWithLastEligible(acc ++ Vector.fill(d)(j / 2), toFill - d)
        }
      }
      fillWithLastEligible(acc, c.asDigit)
    case (acc, (c, i)) if i % 2 == 0 => acc ++ Vector.fill(c.asDigit)(i / 2)
  }

  val answer2 = result2.zipWithIndex.foldLeft(0L) { case (acc, (c, i)) =>
    if c == -1 then acc else acc + i * c
  }
  println(answer2)
//  println(result2.mkString("").replace("-1", "."))
