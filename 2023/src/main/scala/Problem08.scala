import scala.io.Source

object Problem08 extends App:
  val input = Source.fromResource("08-input.txt").getLines().toVector
  val instructions = input.head
  val map = input.drop(2).map {
    case s"$key = ($left, $right)" => key -> (left, right)
  }.toMap

  var done = false
  var i = 0
  var current = "AAA"
  while (!done) {
    val instruction = instructions(i % instructions.length)
    instruction match {
      case 'L' => current = map(current)._1
      case 'R' => current = map(current)._2
    }
    i = i + 1
    done = current == "ZZZ"
  }

  println(i)

  done = false
  i = 0
  var currentGhosts = map.keys.filter(_.endsWith("A")).toVector
  var seen = Vector.fill[Vector[(String, Int)]](currentGhosts.length)(Vector.empty)
  var cycles = Vector.fill[Option[(Int, Int)]](currentGhosts.length)(None)

  while (!done) {
    val instruction = instructions(i % instructions.length)
    instruction match {
      case 'L' => currentGhosts = currentGhosts.map(current => map(current)._1)
      case 'R' => currentGhosts = currentGhosts.map(current => map(current)._2)
    }
    i = i + 1
    cycles = currentGhosts.indices.toVector.map(g =>
      if cycles(g).isDefined then
        cycles(g)
      else if seen(g).contains((currentGhosts(g), i % instructions.length)) then
        Some(seen(g).indexOf((currentGhosts(g), i % instructions.length)), i - seen(g).indexOf((currentGhosts(g), i % instructions.length)) - 1)
      else
        None
    )
    seen = seen.indices.toVector.map(g => seen(g).appended((currentGhosts(g), i % instructions.length)))
    done = cycles.forall(_.isDefined)
  }

  println(i)

  cycles.foreach(println(_))

  def lcm(list: Seq[BigInt]): BigInt = list.foldLeft(1: BigInt) {
    (a, b) => b * a / LazyList.iterate((a, b)) { case (x, y) => (y, x % y) }.dropWhile(_._2 != 0).head._1.abs
  }

  println(lcm(cycles.map(_.get).map(_._2)))