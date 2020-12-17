import scala.io.Source

object Problem17 extends App {

  type World = Array[Array[Array[Char]]]
  type World2 = Array[Array[Array[Array[Char]]]]

  val input = Source.fromResource("17-input.txt").getLines().map(_.toCharArray).toArray
  val world = Array.fill(input.length, input.length, input.length)('.')
  world(world.length / 2) = input

  val world2 = Array.fill(input.length, input.length, input.length, input.length)('.')
  world2(world.length / 2)(world.length / 2) = input

  def shouldIncrease(world: World): Boolean = {
    val max = world.length-1
    (for {
      x <- world.indices
      y <- world.indices
      z <- world.indices
    } yield {
      if (x == 0 || y == 0 || z == 0 || x == max || y == max || z == max)
        world(z)(y)(x) == '#'
      else false
    }).contains(true)
  }

  def shouldIncrease2(world: World2): Boolean = {
    val max = world.length-1
    (for {
      x <- world.indices
      y <- world.indices
      z <- world.indices
      w <- world.indices
    } yield {
      if (x == 0 || y == 0 || z == 0 || w == 0 || x == max || y == max || z == max || w == max)
        world(w)(z)(y)(x) == '#'
      else false
    }).contains(true)
  }

  def increase(world: World): World = {
    if (shouldIncrease(world)) {
      val newSize = world.length + 2
      val newWorld = Array.fill(newSize, newSize, newSize)('.')
      for {
        x <- 1 until newSize - 1
        y <- 1 until newSize - 1
        z <- 1 until newSize - 1
      } yield {
        newWorld(z)(y)(x) = world(z-1)(y-1)(x-1)
      }
      newWorld
    } else world
  }

  def increase2(world: World2): World2 = {
    if (shouldIncrease2(world)) {
      val newSize = world.length + 2
      val newWorld = Array.fill(newSize, newSize, newSize, newSize)('.')
      for {
        x <- 1 until newSize - 1
        y <- 1 until newSize - 1
        z <- 1 until newSize - 1
        w <- 1 until newSize - 1
      } yield {
        newWorld(w)(z)(y)(x) = world(w-1)(z-1)(y-1)(x-1)
      }
      newWorld
    } else world
  }

  def newValue(world: World, x: Int, y: Int, z: Int): Char = {
    val neighbours = (for {
      x <- List(x-1, x, x+1).filter(_ >= 0).filter(_ < world.length)
      y <- List(y-1, y, y+1).filter(_ >= 0).filter(_ < world.length)
      z <- List(z-1, z, z+1).filter(_ >= 0).filter(_ < world.length)
    } yield world(z)(y)(x)).count(_ == '#')

    val correctedNeighbours = if (world(z)(y)(x) == '#') neighbours - 1 else neighbours

    if (world(z)(y)(x) == '#' && (correctedNeighbours == 2 || correctedNeighbours == 3)) '#'
    else if (world(z)(y)(x) == '#') '.'
    else if (world(z)(y)(x) == '.' && correctedNeighbours == 3) '#'
    else world(z)(y)(x)
  }

  def newValue2(world: World2, x: Int, y: Int, z: Int, w: Int): Char = {
    val neighbours = (for {
      x <- List(x-1, x, x+1).filter(_ >= 0).filter(_ < world.length)
      y <- List(y-1, y, y+1).filter(_ >= 0).filter(_ < world.length)
      z <- List(z-1, z, z+1).filter(_ >= 0).filter(_ < world.length)
      w <- List(w-1, w, w+1).filter(_ >= 0).filter(_ < world.length)
    } yield world(w)(z)(y)(x)).count(_ == '#')

    val correctedNeighbours = if (world(w)(z)(y)(x) == '#') neighbours - 1 else neighbours

    if (world(w)(z)(y)(x) == '#' && (correctedNeighbours == 2 || correctedNeighbours == 3)) '#'
    else if (world(w)(z)(y)(x) == '#') '.'
    else if (world(w)(z)(y)(x) == '.' && correctedNeighbours == 3) '#'
    else world(w)(z)(y)(x)
  }

  def countHashes(world: World): Int = {
    world.map(_.map(_.count(_ == '#')).sum).sum
  }

  def countHashes2(world: World2): Int = {
    world.map(_.map(_.map(_.count(_ == '#')).sum).sum).sum
  }

  def printWorld(world: World): Unit = {
    for {
      z <- world.indices
    } yield {
      println(s"z=$z")
      world(z).map(arr => println(arr.mkString("")))
    }
  }

  def printWorld2(world: World2): Unit = {
    for {
      z <- world.indices
      w <- world.indices
    } yield {
      println(s"z=$z, w=$w")
      world(w)(z).map(arr => println(arr.mkString("")))
    }
  }

  println(countHashes(world))
  val endState = (1 to 6).foldLeft(world) {
    case (oldWorld, iteration) =>
      println(s"Iteration $iteration")
      //printWorld(oldWorld)
      val newWorld = increase(oldWorld)
      val worldCopy = newWorld.map(_.map(_.clone))
      for {
        x <- newWorld.indices
        y <- newWorld.indices
        z <- newWorld.indices
      } yield {
        worldCopy(z)(y)(x) = newValue(newWorld, x, y, z)
      }
      worldCopy
  }

  println(countHashes(endState))


  println(countHashes2(world2))
  val endState2 = (1 to 6).foldLeft(world2) {
    case (oldWorld, iteration) =>
      println(s"Iteration $iteration")
      //printWorld(oldWorld)
      val newWorld = increase2(oldWorld)
      val worldCopy = newWorld.map(_.map(_.map(_.clone)))
      for {
        x <- newWorld.indices
        y <- newWorld.indices
        z <- newWorld.indices
        w <- newWorld.indices
      } yield {
        worldCopy(w)(z)(y)(x) = newValue2(newWorld, x, y, z, w)
      }
      worldCopy
  }

  println(countHashes2(endState2))

}
