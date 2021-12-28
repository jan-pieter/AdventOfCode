import scala.io.Source

object Problem25 extends App {

  type World = Array[Array[Char]]
  val input: World = Source.fromResource("25-input.txt").getLines().map(_.toArray).toArray
  input.foreach(line => println(line.mkString))

  def step(world: World): (World, Boolean) = {
    var moved = false
    val newWorld = world.map(_.clone())
    for {
      y <- newWorld.indices
      x <- newWorld.head.indices
    } yield {
      if (world(y)(x) == '>' && world(y)((x+1)%world.head.length) == '.') {
        newWorld(y)(x) = '.'
        newWorld(y)((x+1)%newWorld.head.length) = '>'
        moved = true
      }
    }

    val newWorld2 = newWorld.map(_.clone())
    for {
      y <- newWorld2.indices
      x <- newWorld2.head.indices
    } yield {
      if (newWorld(y)(x) == 'v' && newWorld((y+1)%newWorld.length)(x) == '.') {
        newWorld2(y)(x) = '.'
        newWorld2((y+1)%newWorld2.length)(x) = 'v'
        moved = true
      }
    }
    newWorld2 -> moved
  }

  var stepNr = 1
  var (world, moved) = step(input)
  while (moved) {
    //println(stepNr)
    //world.foreach(line => println(line.mkString))
    stepNr = stepNr + 1
    val tuple = step(world)
    world = tuple._1
    moved = tuple._2
  }

  println(s"Done after $stepNr steps")



}
