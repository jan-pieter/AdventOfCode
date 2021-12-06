import scala.io.Source

object Problem05 extends App {

  case class Line(x1: Int, y1: Int, x2: Int, y2: Int)

  val input: Vector[Line] = Source.fromResource("05-input.txt").getLines().map { line =>
    val splitted = line.split(" -> ").map(_.split(",").map(_.toInt))
    Line(splitted(0)(0), splitted(0)(1), splitted(1)(0), splitted(1)(1))
  }.toVector

//  println(input)

  val map = Array.fill(1000, 1000)(0)

  input.filter(line => line.x1 == line.x2 || line.y1 == line.y2).foreach{line =>
    //println(line)
    for {
      x <- line.x1.min(line.x2) to line.x1.max(line.x2)
      y <- line.y1.min(line.y2) to line.y1.max(line.y2)
    } yield {
      //println(s"Updating $x $y")
      map(y)(x) = map(y)(x) + 1
    }
  }

//  map.foreach(line => println(s"${line.mkString}"))

  // NOT THE RIGHT ANSWER BOT CLOSE ENOUGH TO MANUALLY FIX
  val answer = map.map(line => line.count(_ >= 2)).sum

  println(answer)

  input.filter(line => line.x1 != line.x2 && line.y1 != line.y2).foreach{line =>
    //println(line)
    val length = Math.abs(line.x1-line.x2)
    (0 until length).foreach { i =>
      val x = line.x1 + (if (line.x1 < line.x2) i else -i)
      val y = line.y1 + (if (line.y1 < line.y2) i else -i)
      map(x)(y) = map(x)(y) + 1
    }
  }

  val answer2 = map.map(line => line.count(_ >= 2)).sum

//  map.foreach(line => println(s"${line.mkString}"))


  println(answer2)

}
