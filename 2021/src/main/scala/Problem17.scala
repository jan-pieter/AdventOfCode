import scala.annotation.tailrec
import scala.io.Source

object Problem17 extends App {

//  val (targetX1, targetX2, targetY1, targetY2) = (20, 30, -10, -5)
  //val (targetX1, targetX2, targetY1, targetY2) = (117, 164, -140, -89)

  val target = Source.fromResource("17-input.txt").getLines().next() match {
    case s"target area: x=$x1..$x2, y=$y1..$y2" => Target(x1.toInt, x2.toInt, y1.toInt, y2.toInt)
  }

  case class Target(x1: Int, x2: Int, y1: Int, y2: Int) {
    def inTarget(x: Int, y: Int): Boolean   = x >= x1 && x <= x2 && y >= y1 && y <= y2
    def pastTarget(x: Int, y: Int): Boolean = x > x2 || y < y1
  }

  @tailrec
  def endsInTarget(xPos: Int, yPos: Int, xVelocity: Int, yVelocity: Int, maxY: Int): Option[Int] = {
    val newXPos = xPos + xVelocity
    val newYPos = yPos + yVelocity
    if (target.inTarget(newXPos, newYPos))
      Some(maxY.max(newYPos))
    else if (target.pastTarget(newXPos, newYPos))
      None
    else
      endsInTarget(newXPos, newYPos, 0.max(xVelocity - 1), yVelocity - 1, maxY.max(newYPos))
  }

  //println(s"Ends in target: ${endsInTarget(0, 0, 7, 2, 0)}")

  case class Solution(x: Int, y: Int, maxY: Int)
  val solutions: Seq[Solution] = (for {
    x <- 1 to target.x2
    y <- target.y1 to 500
  } yield endsInTarget(0, 0, x, y, 0).map(maxY => Solution(x, y, maxY))).flatten

  println(s"Answer 1: ${solutions.maxBy(_.maxY)}")
  println(s"Answer 2: ${solutions.size}")

}
