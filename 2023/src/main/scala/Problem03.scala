import scala.io.Source

object Problem03 extends App:
  val input = Source.fromResource("03-input.txt").getLines().toVector
  def isDigit(c: Char): Boolean = c >= '0' && c <= '9'
  def isSymbol(c: Char): Boolean = !isDigit(c) && c != '.'
  def symbolAt(y: Int, x: Int): Boolean =
    Option.when(y >= 0 && y < input.length && x >= 0 && x < input(y).length)(isSymbol(input(y)(x))).getOrElse(false)
  def gearAt(y: Int, x: Int): Boolean =
    Option.when(y >= 0 && y < input.length && x >= 0 && x < input(y).length)(input(y)(x)=='*').getOrElse(false)

  val numbers = for {
    y <- input.indices
    x <- input(y).indices
    if isDigit(input(y)(x)) && (x == 0 || input(y)(x-1) == '.' || isSymbol(input(y)(x-1)))
  } yield {
    val number = input(y).drop(x).takeWhile(isDigit)
    val hasSymbol = symbolAt(y, x-1) || symbolAt(y, x + number.length) || (x-1 to x+number.length).exists(x2 => symbolAt(y-1, x2) || symbolAt(y+1, x2))
    Option.when(hasSymbol)(number.toLong)
  }

  println(numbers.flatten.sum)

  val numbers2 = for {
    y <- input.indices
    x <- input(y).indices
    if isDigit(input(y)(x)) && (x == 0 || input(y)(x - 1) == '.' || isSymbol(input(y)(x - 1)))
  } yield {
    val number = input(y).drop(x).takeWhile(isDigit)
    Option.when(gearAt(y, x-1))((y, x-1) -> number.toLong).toVector ++
      Option.when(gearAt(y, x+number.length))((y, x+number.length) -> number.toLong).toVector ++
      (x - 1 to x + number.length).filter(x2 => gearAt(y-1,x2)).map(x2 => (y-1, x2) -> number.toLong) ++
      (x - 1 to x + number.length).filter(x2 => gearAt(y+1,x2)).map(x2 => (y+1, x2) -> number.toLong)
  }

  val gears = numbers2.flatten.groupBy(_._1).filter(_._2.length == 2).view.mapValues(_.map(_._2).product).toMap

  println(gears.values.sum)