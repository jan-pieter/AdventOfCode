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
    if result1(i) >= 0 then i = i+1
    else if result1(j) < 0 then j = j-1
    else
      val origI = result1(i)
      result1 = result1.updated(i, result1(j)).updated(j, origI)
  }

  def checksum(result: Vector[Int]): Long = result.zipWithIndex.foldLeft(0L) { case (acc, (c, i)) =>
    if c == -1 then acc else acc + i * c
  }

  println(checksum(result1))
//  println(result1.mkString("").replace("-1", "."))

  val initFilesLeft: Vector[(Int, Int)] = input.zipWithIndex.filter((c, i) => i % 2 == 0).map((c, i) => (c.asDigit, i))
  val result2 = input.zipWithIndex.foldLeft((Vector.empty[Int], initFilesLeft)){
    case ((acc, filesLeft), (c, i)) if i % 2 == 1 || filesLeft.forall((d, j) => j != i) =>
      @annotation.tailrec
      def fillWithLastEligible(acc: Vector[Int], toFill: Int, filesLeft: Vector[(Int, Int)]): (Vector[Int], Vector[(Int, Int)]) = {
        val rightEligible = filesLeft.findLast((d, j) => d <= toFill && i < j)
        rightEligible match {
          case None => (acc ++ Vector.fill(toFill)(-1), filesLeft)
          case Some((d, j)) =>
            fillWithLastEligible(
              acc ++ Vector.fill(d)(j / 2),
              toFill - d,
              filesLeft.patch(filesLeft.lastIndexOf((d, j)), Nil, 1)
            )
        }
      }
      fillWithLastEligible(acc, c.asDigit, filesLeft)
    case ((acc, filesLeft), (c, i)) if i % 2 == 0 => (acc ++ Vector.fill(c.asDigit)(i / 2), filesLeft)
  }

  println(checksum(result2._1))
//  println(result2.mkString("").replace("-1", "."))
