import scala.annotation.tailrec
import scala.io.Source

object Problem18 extends App {

  trait Number {
    def level: Int
  }
  case class Pair(left: Number, right: Number, level: Int) extends Number
  case class Literal(value: Int, level: Int) extends Number

  def splitInside(str: String): (String, String) = {
    val result = str.foldLeft((0, false, "", "")) {
      case ((_, true, left, right), char) => (0, true, left, right + char)
      case ((0, false, left, right) , ',') => (0, true, left, right)
      case ((level, false, left, right), '[') => (level + 1, false, left + '[', right)
      case ((level, false, left, right), ']') => (level - 1, false, left + ']', right)
      case ((level, false, left, right), char) => (level, false, left + char, right)
    }
    result._3 -> result._4
  }

  val LiteralPattern = "^([0-9]+)$".r
  val PairPattern = "^\\[(.*)\\]$".r
  def parseNumber(str: String, level: Int): Number = str match {
    case LiteralPattern(value) => Literal(value.toInt, level)
    case PairPattern(inside) =>
      val (left, right) = splitInside(inside)
      Pair(parseNumber(left, level+1), parseNumber(right, level+1), level)
  }

  val input = Source.fromResource("18-input.txt").getLines().map(line => parseNumber(line, 0)).toVector

//  input.foreach(println(_))

  def maxLevel(number: Number): Int = number match {
    case Literal(_, level) => level
    case Pair(left, right, _) => maxLevel(left).max(maxLevel(right))
  }

  def maxValue(number: Number): Int = number match {
    case Literal(value, _) => value
    case Pair(left, right, _) => maxValue(left).max(maxValue(right))
  }

  @tailrec
  def reduce(number: Number): Number = {
    if (maxLevel(number) > 4)
      reduce(explode(number))
    else if (maxValue(number) >= 10)
      reduce(split(number))
    else
      number
  }

  def addToLeftMost(number: Number, toAdd: Int): Number = number match {
    case Literal(value, level) => Literal(value+toAdd, level)
    case Pair(left, right, level) => Pair(addToLeftMost(left, toAdd), right, level)
  }
  def addToRightMost(number: Number, toAdd: Int): Number = number match {
    case Literal(value, level) => Literal(value+toAdd, level)
    case Pair(left, right, level) => Pair(left, addToRightMost(right, toAdd), level)
  }

  def explode(number: Number): Number = {
    def explodeImpl(number: Number): (Boolean, Option[Int], Option[Int], Number) = number match {
      case x: Literal =>
        (false, None, None, x)
      case p@Pair(left: Literal, right: Literal, level) if level == 4 =>
        (true, Some(left.value), Some(right.value), Literal(0, level))
      case Pair(left, right, level) if maxLevel(left) > 4 =>
        val explodedLeft = explodeImpl(left)
        (true, explodedLeft._2, None, Pair(explodedLeft._4, explodedLeft._3.map(toAdd => addToLeftMost(right, toAdd)).getOrElse(right), level))
      case Pair(left, right, level) if maxLevel(right) > 4 =>
        val explodedRight = explodeImpl(right)
        (true, None, explodedRight._3, Pair(explodedRight._2.map(toAdd => addToRightMost(left, toAdd)).getOrElse(left), explodedRight._4, level))
      case p: Pair =>
        (false, None, None, p)
    }
    explodeImpl(number)._4
  }

  def split(number: Number): Number = number match {
    case Literal(value, level) if value >= 10 => Pair(Literal(value / 2, level+1), Literal(value / 2 + (if (value % 2 == 0) 0 else 1), level+1), level)
    case l@Literal(_, _) => l
    case Pair(left, right, level) =>
      val splittedLeft = split(left)
      val splittedRight = if(splittedLeft == left) split(right) else right
      Pair(splittedLeft, splittedRight, level)
  }

  def toString(number: Number): String = number match {
    case Literal(value, level) => value.toString
    case Pair(left, right, level) => "[" + toString(left) + "," + toString(right) + "]"
  }

  def magnitude(number: Number): Long = number match {
    case Literal(value, _) => value.toLong
    case Pair(left, right, _) => 3 * magnitude(left) + 2 * magnitude(right)
  }

  def increaseLevel(number: Number): Number = number match {
    case Literal(value, level) => Literal(value, level+1)
    case Pair(left, right, level) => Pair(increaseLevel(left), increaseLevel(right), level + 1)
  }

  def add(left: Number, right: Number): Number = reduce(Pair(increaseLevel(left), increaseLevel(right), 0))

  val finalNumber = input.reduce(add)
  println(toString(finalNumber))
  println(s"Answer1: ${magnitude(finalNumber)}")

  val answer2: Long = input.map{ number1 =>
    input.filter(_!=number1).map { number2 =>
      val result = add(number1, number2)
      val m = magnitude(result)
      //println(s"${toString(number1)} + ${toString(number2)} : ${toString(result)} : $m")
      m
  }.max}.max
  println(s"Answer2: $answer2")


}
