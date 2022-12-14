import scala.io.Source

object Problem14 extends App:
  val input: Vector[Vector[(Int, Int)]] = Source.fromResource("14-input.txt").getLines().toVector.map(_.split(" -> ").toVector.map {
    case s"$x,$y" => x.toInt -> y.toInt
  })

  val maxDepth = input.map(_.map(_._2).max).max

  val cave = Array.fill(maxDepth+2, 1000)('.')

//  input.foreach(println(_))
  // Add rock
  input.foreach(segments => segments.sliding(2).foreach{
    case Vector((x1, y1), (x2, y2)) =>
      for {
      x <- x1 to x2 by (if (x1 <= x2) 1 else -1)
      y <- y1 to y2 by (if (y1 <= y2) 1 else -1)
    } yield { cave(y)(x) = '#' }
  })

//  cave.foreach(line => println(line.mkString))

  // Fill with sand
  var done = false
  var sand = 0L
  while (!done) {
    var sandPos = (500, 0)
    var sandAtRest = false
    while (!sandAtRest && sandPos._2 < cave.length - 1) {
      if (cave(sandPos._2 + 1)(sandPos._1) == '.') {
        sandPos = sandPos._1 -> (sandPos._2 + 1)
      } else if (cave(sandPos._2 + 1)(sandPos._1 - 1) == '.') {
        sandPos = (sandPos._1 - 1) -> (sandPos._2 + 1)
      } else if (cave(sandPos._2 + 1)(sandPos._1 + 1) == '.') {
        sandPos = (sandPos._1 + 1) -> (sandPos._2 + 1)
      } else {
        sandAtRest = true
        cave(sandPos._2)(sandPos._1) = 'o'
        sand += 1L
      }
    }
    done = !sandAtRest
  }

  println(sand)

  val cave2 = Array.fill(maxDepth+3, 1000)('.')
  cave2.head.indices.foreach(i => cave2(maxDepth+2)(i) = '#')

  input.foreach(segments => segments.sliding(2).foreach {
    case Vector((x1, y1), (x2, y2)) =>
      //      println(s"$x1,$y1 -> $x2,$y2")
      for {
        x <- x1 to x2 by (if (x1 <= x2) 1 else -1)
        y <- y1 to y2 by (if (y1 <= y2) 1 else -1)
      } yield {
        cave2(y)(x) = '#'
      }
  })

//  cave2.foreach(line => println(line.mkString))

  // Fill with sand
  var done2 = false
  var sand2 = 0L
  while (!done2) {
    var sandPos = (500, 0)
    var sandAtRest = false
    while (!sandAtRest && sandPos._2 < cave2.length - 1) {
      if (cave2(sandPos._2 + 1)(sandPos._1) == '.') {
        sandPos = sandPos._1 -> (sandPos._2 + 1)
      } else if (cave2(sandPos._2 + 1)(sandPos._1 - 1) == '.') {
        sandPos = (sandPos._1 - 1) -> (sandPos._2 + 1)
      } else if (cave2(sandPos._2 + 1)(sandPos._1 + 1) == '.') {
        sandPos = (sandPos._1 + 1) -> (sandPos._2 + 1)
      } else {
        sandAtRest = true
        cave2(sandPos._2)(sandPos._1) = 'o'
        sand2 += 1L
      }
    }
    done2 = sandPos == (500, 0)
  }

//  println()
//  cave2.foreach(line => println(line.mkString))

  println(sand2)
