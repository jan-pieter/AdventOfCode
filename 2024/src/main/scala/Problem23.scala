import scala.io.Source

object Problem23 extends App:
//  val file = "23-test.txt"
  val file = "23-input.txt"
  val input = Source.fromResource(file).getLines().toVector
  val connections = input.map {
    case s"$a-$b" => (a, b)
  }.toSet
  val allNodes = connections.flatMap(connection => Set(connection._1, connection._2)).toVector
  val result = allNodes.combinations(3).filter {
    case Vector(a, b, c) =>
      (a.startsWith("t") || b.startsWith("t") || c.startsWith("t")) && (
      (connections.contains((a, b)) || connections.contains(b, a)) &&
        (connections.contains((b, c)) || connections.contains(c, b)) &&
        (connections.contains((c, a)) || connections.contains(a, c)))
  }
  println(result.size)

  val neighbors: Map[String, Set[String]] = connections.foldLeft(Map.empty[String, Set[String]]) { case (acc, (a, b)) =>
    acc.updated(a, acc.getOrElse(a, Set.empty) + b).updated(b, acc.getOrElse(b, Set.empty) + a)
  }

  /*
  algorithm BronKerbosch1(R, P, X) is
    if P and X are both empty then
        report R as a maximal clique
    for each vertex v in P do
        BronKerbosch1(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
        P := P \ {v}
        X := X ⋃ {v}
  */
  def bronKerbosch1(r: Set[String], p: Set[String], x: Set[String]): Set[String] =
    if p.isEmpty && x.isEmpty then r
    else
      p.foldLeft((Set.empty[String], p, x)) { case (acc, v) =>
        val (result, newP, newX) = acc
        val maxClique = bronKerbosch1(r + v, newP.intersect(neighbors(v)), newX.intersect(neighbors(v)))
        (Vector(result, maxClique).maxBy(_.size), newP - v, newX + v)
      }._1

  val result2 = bronKerbosch1(Set.empty, allNodes.toSet, Set.empty)
  println(result2.toVector.sorted.mkString(","))