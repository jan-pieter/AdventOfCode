import scala.io.Source

object Problem11 extends App {

  val input = Source.fromResource("11-input.txt").getLines().toArray.map(_.toArray)

  def withinBounds[A](map: Array[Array[A]], x: Int, y: Int): Boolean =
    x >= 0 && y >= 0 && x < map(0).length && y < map.length

  def iterate(map: Array[Array[Char]]): Array[Array[Char]] = {
    val newMap = Array.fill(input.length, input(0).length)('.')
    for {
      y <- newMap.indices
      x <- newMap(0).indices
    } yield {
      val taken = (for {
        diffY <- -1 to 1 if y + diffY >= 0 && y + diffY < newMap.length
        diffX <- -1 to 1 if x + diffX >= 0 && x + diffX < newMap(0).length
      } yield {
        if (diffY == 0 && diffX == 0)
          0
        else if (map(y + diffY)(x + diffX) == '#')
          1
        else
          0
      }).sum
      //println(s"$x $y $taken")
      if (map(y)(x) == 'L' && taken == 0)
        newMap(y)(x) = '#'
      else if (map(y)(x) == '#' && taken >= 4)
        newMap(y)(x) = 'L'
      else
        newMap(y)(x) = map(y)(x)
    }
    newMap
  }

  def iterate2(map: Array[Array[Char]], debug: Boolean = false): Array[Array[Char]] = {
    val newMap = Array.fill(input.length, input(0).length)('.')
    for {
      y <- newMap.indices
      x <- newMap(0).indices
    } yield {
      val taken = (for {
        diffY <- -1 to 1 if y + diffY >= 0 && y + diffY < newMap.length
        diffX <- -1 to 1 if x + diffX >= 0 && x + diffX < newMap(0).length
      } yield {
        if (diffY == 0 && diffX == 0) {
          0
        } else {
          var dirY = diffY
          var dirX = diffX
          while (withinBounds(map, x+dirX, y+dirY) && map(y + dirY)(x + dirX) == '.') {
            dirY += diffY
            dirX += diffX
          }
          if (debug) {
            println(s"$x $y $diffX $diffY $dirX $dirY")
          }
          if (!withinBounds(map, x+dirX, y+dirY))
            0
          else if (map(y+dirY)(x+dirX) == '#')
            1
          else
            0
        }
      }).sum
//      println(s"$x $y $taken")
      if (map(y)(x) == 'L' && taken == 0)
        newMap(y)(x) = '#'
      else if (map(y)(x) == '#' && taken >= 5)
        newMap(y)(x) = 'L'
      else
        newMap(y)(x) = map(y)(x)
    }
    newMap
  }

  var currentMap = input
  var newMap = iterate(currentMap)

//  currentMap.foreach(line => println(line.mkString("")))
//  println("")
//  newMap.foreach(line => println(line.mkString("")))
//  println("")

  while(currentMap.deep != newMap.deep) {
    currentMap = newMap
    newMap = iterate(currentMap)
//    newMap.foreach(line => println(line.mkString("")))
//    println("")
  }

  val seats = currentMap.map(_.count(_ == '#')).sum
  println("")
  println(seats)

  currentMap = input
  newMap = iterate2(currentMap)

//  currentMap.foreach(line => println(line.mkString("")))
//  println("")
//  newMap.foreach(line => println(line.mkString("")))
//  println("")

  while(currentMap.deep != newMap.deep) {
    currentMap = newMap
    newMap = iterate2(currentMap)
//    newMap.foreach(line => println(line.mkString("")))
//    println("")
  }

  val seats2 = currentMap.map(_.count(_ == '#')).sum
  println("")
  println(seats2)

}
