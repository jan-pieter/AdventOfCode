import scala.io.Source

object Problem20 extends App {

  val input = Source.fromResource("20-input.txt").getLines().toVector
  def toBoolean(char: Char) = char match {
    case '#' => true
    case '.' => false
  }
  def toInteger(seq: Seq[Boolean]): Int = {
    seq.foldLeft(0){
      case (n, b) => (n << 1) + (if (b) 1 else 0)
    }
  }
  val algorithm: Vector[Boolean] = input.take(1).head.map(toBoolean).toVector
  val image: Array[Array[Boolean]] = input.drop(2).map(_.map(toBoolean).toArray).toArray

  def step(oldImage: Array[Array[Boolean]], default: Boolean): Array[Array[Boolean]] = {
    val newImage = Array.ofDim[Boolean](oldImage.length + 2, oldImage.head.length + 2)
    for {
      y <- newImage.indices
      x <- newImage.head.indices
    } yield {
      val square: Seq[Boolean] = for {
        diffY <- -1 to 1
        diffX <- -1 to 1
      } yield {
//        println(s"For $y $x looking at ${y - 1 + diffY} ${x - 1 + diffX}")
        oldImage.lift(y - 1 + diffY).flatMap(_.lift(x - 1 + diffX)).getOrElse(default)
      }
      newImage(y)(x) = algorithm(toInteger(square))
    }
    newImage
  }

  def printImage(image: Array[Array[Boolean]]): Unit = {
    image.foreach(line => println(line.map(b => if (b) '#' else '.').mkString))
  }

  printImage(image)
  println()
  val step1 = step(image, default = false)
  printImage(step1)
  println()
  val step2 = step(step1, default = algorithm.head)
  printImage(step2)

  val answer1 = step2.map(_.count(identity)).sum
  println(s"Answer1: $answer1")

  val result2 = (1 to 50).foldLeft(image) {
    case (result, stepNr) => step(result, default = if (stepNr % 2 == 0) algorithm.head else false)
  }
  val answer2 = result2.map(_.count(identity)).sum
  println(s"Answer2: $answer2")

}
