import scala.io.Source

object Problem13 extends App {
  case class Point(x: Int, y: Int)
  sealed trait Fold
  case class Horizontal(y: Int) extends Fold
  case class Vertical(x: Int) extends Fold

  val input = Source.fromResource("13-input.txt").getLines().toVector
  val (dots, folds) = input.splitAt(input.indexOf("")) match {
    case (part1, part2) => part1.map {
      case s"$x,$y" => Point(x.toInt, y.toInt)
    } -> part2.drop(1).map {
      case s"fold along x=$x" => Vertical(x.toInt)
      case s"fold along y=$y" => Horizontal(y.toInt)
    }
  }

  val paper: Array[Array[Boolean]] = Array.fill(dots.map(_.y).max + 1, dots.map(_.x).max + 1)(false)

  dots.foreach {
    case Point(x, y) => paper(y)(x) = true
  }

//  print(paper)

  def print(paper: Array[Array[Boolean]]): Unit = {
    paper.foreach(line => println(line.map {
      case true => '#'
      case false => '.'
    }.mkString))
  }

  def foldAlong(paper: Array[Array[Boolean]], instruction: Fold): Array[Array[Boolean]] = instruction match {
    case Vertical(x) => foldAlong(paper.clone.transpose, Horizontal(x)).transpose
    case Horizontal(y) =>
      val (top, bottom) = paper.clone.splitAt(y) match {
        case (part1, part2) => part1 -> part2.drop(1)
      }
      if (top.length >= bottom.length) {
        for {
          bottomY <- bottom.indices
          bottomX <- bottom.head.indices
        } yield {
          if (bottom(bottomY)(bottomX)) {
            top(y - bottomY - 1)(bottomX) = true
          }
        }
        top
      } else {
        throw new IllegalArgumentException("Mismatch!")
      }
  }

//  println("")
//  val result = foldAlong(paper, folds.head)
//  print(result)

//  val solution1 = result.map(_.count(identity)).sum
//  println(s"Solution1: $solution1")

  val result = folds.foldLeft(paper) {
    case (paper, fold) => foldAlong(paper, fold)
  }

  print(result)


}
