import scala.io.Source

object Problem17 extends App:
  val input = Source.fromResource("17-input.txt").getLines().toVector.head

  val rocks = Vector(
    Vector("####"),
    Vector(".#.","###",".#."),
    Vector("..#","..#","###"),
    Vector("#","#","#","#"),
    Vector("##","##")
  )

  val steps = 10000
  val world = Array.fill(4*steps + 2, 7)('.')
  world.head.indices.foreach(world(world.length-1)(_) = '#')
  var topStack = world.length - 1
  var rockIndex = 0
  var gasIndex = 0
  var previous = 0
  var previousHeight = 0

  def printWorld: Unit = {
    world.filterNot(_.forall(_=='.')).foreach(l => println(l.mkString))
  }

  def rockFits(rock: Vector[String], rockPos: (Int, Int)): Boolean = {
    rock.indices.forall(y => rock.head.indices.forall(x => world(rockPos._2 + y)(rockPos._1 + x) == '.' || rock(y)(x) == '.'))
  }

//  def rockFits(rock: Vector[String], rockPos: (Int, Int)): Boolean = {
//    rock.indices.forall(y => world(rockPos._2 + y)(rockPos._1) == '.' && world(rockPos._2 + y)(rockPos._1 + rock.head.length - 1) == '.')
//  }

  while (rockIndex < steps) {
    val rock = rocks(rockIndex % rocks.length)
    var rockPos = (2, topStack - 3 - rock.length)
    var done = false
    while (!done) {
      // Push aside
      input(gasIndex % input.length) match {
        case '<' if rockPos._1 > 0 && rockFits(rock, (rockPos._1 - 1) -> rockPos._2) => rockPos = (rockPos._1 - 1) -> rockPos._2
        case '>' if rockPos._1 < (world.head.length - rock.head.length) && rockFits(rock, (rockPos._1 + 1) -> rockPos._2) => rockPos = (rockPos._1 + 1) -> rockPos._2
        case _ => //no-op
      }
      gasIndex += 1
      // Down
      if (rockFits(rock, rockPos._1 -> (rockPos._2 + 1))) {
        rockPos = rockPos._1 -> (rockPos._2 + 1)
      } else {
        done = true
      }
    }
    rock.indices.foreach(y => rock(y).indices.foreach(x => if (rock(y)(x) == '#') world(rockPos._2 + y)(rockPos._1 + x) = '#'))
    while (world(topStack - 1).contains('#')) topStack = topStack - 1
    rockIndex += 1
    if (rockIndex - previous == 909) {
      println(s"heightincrease ${world.length - topStack - 1 - previousHeight}")
    }
    if (world(topStack).forall(_ == '#')) {
      println(s"Filled after $rockIndex at ${world.length - topStack - 1} diff ${rockIndex - previous} and ${world.length - topStack - 1 - previousHeight}")
      previous = rockIndex
      previousHeight = world.length - topStack - 1
      if (rockIndex % rocks.length == 0 ){ //&& gasIndex % input.length == 0) {
        println("Repetition!")
      }
    }
  }
  println(gasIndex)
  println(world.length - topStack - 1)

  // 2524 + (1000000000000 / 1750) * 2796 + 1454
  // remainder 909

  println(2524L + ((1000000000000L - 1591L) / 1750L) * 2796L + 1454L)

//  val solution1 = input.map(_.sum).max
//  println(solution1)

//  val solution2 = input.map(_.sum).sorted.takeRight(3).sum
//  println(solution2)
