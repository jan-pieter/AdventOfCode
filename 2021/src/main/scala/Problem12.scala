import scala.io.Source

object Problem12 extends App {

  val input: Vector[(String, String)] = Source.fromResource("12-input.txt").getLines().map{ line =>
    val parts = line.split("-")
    parts(0) -> parts(1)
  }.toVector

//  input.foreach(println(_))

//  println("")

  def connectedCaves(cave: String): Set[String] = {
    input.filter(edge => edge._1 == cave || edge._2 == cave).map(edge => Set(edge._1, edge._2)).foldLeft(Set.empty[String])(_ ++ _) - cave
  }

  def findPaths(cave: String, visited: Set[String]): Set[List[String]] = {
    if (cave == "end"){
      Set(List(cave))
    } else {
      val newVisited = if (cave.forall(_.isUpper)) visited else visited + cave
      val connected: Set[String] = connectedCaves(cave).diff(visited)
      val paths: Set[List[String]] = connected.flatMap(connected => findPaths(connected, newVisited))
      paths.map(cave :: _)
    }
  }

  val solution1 = findPaths("start", Set.empty)

//  solution1.foreach(println(_))

  println(s"Solution1: ${solution1.size}")

  def findExtendedPaths(cave: String, visited: Set[String], visitedOnce: Option[String]): Set[List[String]] = {
    if (cave == "end"){
      Set(List(cave))
    } else {
      val connected: Set[String] = connectedCaves(cave).diff(visited)
      val paths: Set[List[String]] = if (cave.forall(_.isUpper))
        connected.flatMap(connected => findExtendedPaths(connected, visited, visitedOnce))
      else if (visitedOnce.isDefined || cave == "start")
        connected.flatMap(connected => findExtendedPaths(connected, visited + cave, visitedOnce))
      else
        connected.flatMap(connected => findExtendedPaths(connected, visited, Some(cave))) ++ connected.flatMap(connected => findExtendedPaths(connected, visited + cave, visitedOnce))

      paths.map(cave :: _)
    }
  }

  val solution2 = findExtendedPaths("start", Set.empty, None)

  //solution2.toVector.sortBy(_.mkString.toLowerCase).foreach(println(_))

  println(s"Solution2: ${solution2.size}")

}
