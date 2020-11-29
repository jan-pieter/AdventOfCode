import scala.collection.mutable
import scala.io.Source

object Problem18 extends App {

  val world: Array[Array[Char]] = Source.fromResource("18-test.txt").getLines().map(_.toCharArray).toArray

  val start: (Int, Int) = world.map(_.indexOf('@')).zipWithIndex.maxBy(_._1)
  println(s"Start $start")

  def mergeMaps(map1: Map[Char, Set[Char]], map2: Map[Char,Set[Char]]): Map[Char,Set[Char]] = {
    val merged = map1.toSeq ++ map2.toSeq
    val grouped = merged.groupBy(_._1)
    grouped.mapValues(_.map(_._2).toSet.flatten)
  }

  def shields(location: (Int, Int), beenTo: Set[(Int, Int)], doors: Set[Char]): Map[Char, Set[Char]] = {
    val result = for {
      (x, y) <- List((-1,0),(1,0),(0,-1),(0,1))
    } yield {
      val afterStep = (location._1+x, location._2+y)
      if (afterStep._1 >= 0 && afterStep._2 >= 0 && afterStep._1 < world(0).length && afterStep._2 < world.length && !beenTo.contains(afterStep)) {
        world(afterStep._2)(afterStep._1) match {
          case '.' | '@' => shields(afterStep, beenTo + location, doors)
          case '#' => Map.empty[Char, Set[Char]]
          case key if key.isLower => mergeMaps(shields(afterStep, beenTo + location, doors), doors.map(_ -> Set(key)).toMap)
          case key => shields(afterStep, beenTo + location, doors + key)
        }
      } else {
        Map.empty[Char, Set[Char]]
      }
    }
    result.foldLeft(Map.empty[Char,Set[Char]])((map1, map2) => mergeMaps(map1, map2))
  }

  val shieldsAtStart = shields(start, Set.empty, Set(' '))
  println(s"Shields at start: $shieldsAtStart")

  def reachable(location: (Int, Int), visited: Set[Char]): List[(Char, Int, (Int, Int))] = {
    val toVisit: mutable.Set[((Int, Int), Int)] = mutable.Set(location -> 0)
    val beenTo: mutable.Set[(Int,Int)] = mutable.Set.empty
    val canAccess = mutable.Set[(Char,Int,(Int, Int))]()
    def takeStep(elem: ((Int, Int), Int)): Unit = {
      val (newLocation, distance) = elem
      toVisit.remove(elem)
      for {
        (x, y) <- List((-1,0),(1,0),(0,-1),(0,1))
      } yield {
        val afterStep = (newLocation._1+x, newLocation._2+y)
        if (afterStep._1 >= 0 && afterStep._2 >= 0 && afterStep._1 < world(0).length && afterStep._2 < world.length && !beenTo.contains(afterStep)) {
          beenTo.add(afterStep)
          world(afterStep._2)(afterStep._1) match {
            case '.' | '@' => toVisit.add(afterStep -> (distance + 1))
            case '#' => //no-op
            case key if key.isLower && !visited.contains(key) => canAccess.add((key, (distance + 1), afterStep))
            case key if key.isLower => toVisit.add(afterStep -> (distance + 1))
//            case key if visited.contains(key) => toVisit.add(afterStep -> (distance + 1))
            case key if visited.contains(key.toLower) => toVisit.add(afterStep -> (distance + 1)) //canAccess.add((key, (distance + 1), afterStep))
            case _ => //no-op
          }
        }
      }
    }
    while (toVisit.nonEmpty)
      takeStep(toVisit.head)
    canAccess.toList.sortBy(_._2)
  }
  println(s"Reachable: ${reachable(start, Set.empty)}")
  //System.exit(0)

  //println(s"Reachable2: ${reachable((7,1), Set('a'))}")

  var bestSolution = 3000

  def solve(location: (Int, Int), visited: String, steps: Int): Option[(Int, String)] = {
    if (steps >= bestSolution) {
      None
    } else {
      val r = reachable(location, visited.toSet)
      if (r.nonEmpty) {
        val bestNext = r.flatMap { triple =>
          solve(triple._3, visited + triple._1, steps+triple._2).map(result =>
            (result._1 + triple._2) -> (triple._1 + result._2))
        }.sortBy(_._1).headOption
        bestNext
      } else {
        println(s"$visited $steps")
        bestSolution = steps
        Some((0, ""))
      }
    }
  }

  def options(location: (Int, Int), visited: String): Int = {
    val r = reachable(location, visited.toSet)
    if (r.nonEmpty)
      r.map { triple =>
        val (key, _, position) = triple
        options(position, visited + key)
      }.sum
    else
      1
  }

  val totalOptions = options(start, "")
  println(s"Options: $totalOptions")

//  val solution = solve(start, "", 0)
//  println(s"Solution $solution")

}
