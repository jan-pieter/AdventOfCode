import scala.io.Source

object Problem14 extends App:
  type Dish = Vector[Vector[Char]]
  val input: Dish = Source.fromResource("14-input.txt").getLines().toVector.map(_.toVector)

  def tiltWest(dish: Dish): Dish = dish.map{ line =>
    line.indices.foldLeft(line){
      case (soFar, i) if soFar(i) != '.' => soFar
      case (soFar, i) =>
        val firstCube: Int = soFar.drop(i+1).indexOf('#')
        val firstRound: Int = soFar.drop(i+1).indexOf('O')
        val toSwap: Option[Int] = (firstCube, firstRound) match {
          case (_, -1) => None
          case (-1, r) => Some(r+i+1)
          case (c, r) if r < c => Some(r+i+1)
          case _ => None
        }
        toSwap.map(r => soFar.updated(i, 'O').updated(r, '.')).getOrElse(soFar)
    }
  }

  def load(dish: Dish): Long =
    dish.reverse.zipWithIndex.map((line, i) => line.count(_=='O') * (i+1)).sum

  def tiltNorth(dish: Dish): Dish = tiltWest(dish.transpose).transpose
  def tiltSouth(dish: Dish): Dish = tiltNorth(dish.reverse).reverse
  def tiltEast(dish: Dish): Dish = tiltWest(dish.map(_.reverse)).map(_.reverse)

  val result1 = tiltNorth(input)
//  result1.foreach(line => println(line.mkString))
//  println()
  println(load(result1))

  def cycle(dish: Dish): Dish = tiltEast(tiltSouth(tiltWest(tiltNorth(dish))))

  // Nasty mutable code to detect the cycle... protect your eyes :P
  var done = false
  var i = 0
  var dish = input
  var seen = Vector(input)
  var cycleStart = -1
  var cycleEnd = -1
  while (!done) {
    dish = cycle(dish)
    i = i + 1
    val before = seen.indexOf(dish)
    if (before != -1) {
      done = true
      cycleStart = before
      cycleEnd = i
    }
    seen = seen.appended(dish)
  }
  println(s"Cycle $cycleStart => $cycleEnd")

  val endDish = seen(cycleStart + ((1000000000-cycleStart) % (cycleEnd-cycleStart)))
//  println(seen.map(load))
//  result2.foreach(line => println(line.mkString))
  println(load(endDish))
