import scala.annotation.tailrec

object Problem17 extends App {

//  val (targetX1, targetX2, targetY1, targetY2) = (20, 30, -10, -5)
  val (targetX1, targetX2, targetY1, targetY2) = (117, 164, -140, -89)

  @tailrec
  def endsInTarget(xPos: Int, yPos: Int, x: Int, y: Int, maxY: Int): (Boolean, Int) = {
//    println(s"$xPos $yPos $x $y")
    val newXPos = xPos + x
    val newYPos = yPos + y
    if (newXPos >= targetX1 && newXPos <= targetX2 && newYPos >= targetY1 && newYPos <= targetY2)
      true -> Math.max(maxY, newYPos)
    else if (newXPos > targetX2 || newYPos < targetY1)
      false -> -1
    else
      endsInTarget(newXPos, newYPos, Math.max(0, x - 1), y - 1, Math.max(maxY, newYPos))
  }

  println(s"Ends in target: ${endsInTarget(0, 0, 7, 2, 0)}")

  val answer: Seq[(Int, Int, (Boolean, Int))] = (for {
    x <- 1 to targetX2
    y <- targetY1 to 500
  } yield (x, y, endsInTarget(0, 0, x, y, 0))).filter(_._3._1)

  println(s"Answer 1: ${answer.maxBy(_._3._2)}")
  println(s"Answer 2: ${answer.size}")

}
