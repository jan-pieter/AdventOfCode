import scala.io.Source

object Problem10 extends App {

  val map: Vector[Vector[Boolean]] = Source.fromResource("10-input.txt").getLines().map(_.map(_ == '#').toVector).toVector

  //map.map(line => println(line.map(v => if (v) "#" else ".").mkString))

  def gcd(x: Int, y: Int): Int = {
    val bigX = BigInt(x)
    val bigY = BigInt(y)
    bigX.gcd(bigY).intValue
  }

  def detectedAsteroids(x: Int, y: Int): Int = {
    //println(s"Detecting asteroids for $x,$y")
    var updatedMap = map
    for {
      newY <- map.indices
      newX <- map.head.indices
    } yield {
      if (map(newY)(newX) && !(newX == x && newY == y)) {
        var updateX = newX
        var updateY = newY
        val diffX = newX - x
        val diffY = newY - y
        val stepX = if (diffX == 0) 0 else diffX / gcd(diffX, diffY)
        val stepY = if (diffY == 0) 0 else diffY / gcd(diffX, diffY)
        //println(s"$newX $newY $diffX $diffY $stepX $stepY")
        updateX += stepX
        updateY += stepY
        while (updateX >= 0 && updateX < map.head.size && updateY >= 0 && updateY < map.size) {
          //println(s"Setting $updateX $updateY to false")
          updatedMap = updatedMap.updated(updateY, updatedMap(updateY).updated(updateX, false))
          updateX += stepX
          updateY += stepY
        }
      }
    }
    updatedMap.map(_.count(identity)).sum - 1
  }

  def detectedAsteroids2(x: Int, y: Int): Int = {
    //println(s"Detecting asteroids for $x,$y")
    val angles = for {
      newY <- map.indices
      newX <- map.head.indices
    } yield {
      if (map(newY)(newX) && !(newX == x && newY == y)) {
        val diffX: Double = newX - x
        val diffY: Double = newY - y
        val result = Math.atan2(diffX, diffY)
        //println(s"$newX $newY $diffX $diffY $result")
        result
      }
    }
    angles.toSet.size - 1
  }

  val detected: Seq[(Int, (Int, Int))] = for {
    y <- map.indices
    x <- map.head.indices
  } yield {
    if (map(y)(x))
      detectedAsteroids2(x, y) -> (x, y)
    else
      0 -> (x,y)
  }

  //detected.grouped(map.head.size).toList.map(line => println(line.mkString))
  val bestAsteroid = detected.maxBy(_._1)
  println(bestAsteroid)

  case class Asteroid(angle: Double, distance: Double, x: Int, y: Int)
  def otherAsteroids(x: Int, y: Int): Seq[Asteroid] = {
    println(s"Detecting asteroids for $x,$y")
    (for {
      newY <- map.indices
      newX <- map.head.indices
    } yield {
      if (map(newY)(newX) && !(newX == x && newY == y)) {
        val diffX: Double = newX - x
        val diffY: Double = newY - y
        val angle = Math.atan2(diffX, diffY)
        val distance = Math.sqrt(diffX*diffX + diffY*diffY)
        Some(Asteroid(angle, distance, newX, newY))
      } else None
    }).flatten
  }

  val result: Vector[Asteroid] = otherAsteroids(bestAsteroid._2._1, bestAsteroid._2._2).groupBy(_.angle).view.mapValues(_.sortBy(_.distance).zipWithIndex).values.flatten.toVector.sortBy(tuple => (tuple._2, tuple._1.angle * -1)).map(_._1)

  //result.foreach(item => println(item))
//  println(result(0))
//  println(result(1))
//  println(result(2))
//  println(result(9))
//  println(result(19))
//  println(result(49))
//  println(result(99))
//  println(result(198))
  println(result(199))
//  println(result(200))
//  println(result(298))

  println(result(199).x*100 + result(199).y)

}
