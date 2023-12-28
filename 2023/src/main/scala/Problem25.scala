import Problem25.graph
import scalax.collection.edges.UnDiEdge
import scalax.collection.mutable.Graph

import scala.io.Source

object Problem25 extends App:
  val input = Source.fromResource("25-input.txt").getLines().toVector.flatMap {
    case s"$component: $others" => others.split(" ").toVector.map(other => component -> other)
  }
  val graph = Graph.empty[String, UnDiEdge[String]]
  input.foreach { (from, to) =>
    graph.add(from)
    graph.add(to)
    graph.add(UnDiEdge(from, to))
  }
  println(graph.isConnected)
  println(graph.nodes.toVector.size)
  println(graph.edges.toVector.size)

  val combinations = graph.nodes.toVector.combinations(2).toVector
  println(s"${combinations.size} combinations")
  var i = 0
  val start = System.nanoTime()
  val results = combinations.foldLeft(Map.from(graph.edges.map(_ -> 0))) {
    case (acc, Vector(from, to)) =>
      i = i + 1
      val path = graph.get(from).shortestPathTo(to).get.edges
      if (i % 10000 == 0) println(".")
      path.foldLeft(acc)((inner, edge) => inner.updated(edge, inner(edge)+1))
  }
  println("Calculated paths")
//  results.foreach(println(_))
//  val toCut = results.flatten.groupBy(identity).toVector.sortBy(_._2.length * -1).take(3).map(_._1)
  val toCut = results.toVector.sortBy(_._2 * -1).take(3).map(_._1)
  println(toCut)

  toCut.foreach(edge => graph.remove(edge))
  println(graph.isConnected)
  val subgraph1 = graph.get(toCut.head.source).weakComponent
  val subgraph2 = graph.get(toCut.head.target).weakComponent
  println(subgraph1.nodes.size * subgraph2.nodes.size)

  def allShotestPaths = {
    val nodeIndex = graph.nodes.zipWithIndex.toMap
    val dist = Array.fill(graph.nodes.size, graph.nodes.size)(Int.MaxValue)
    val prev = Array.fill(graph.nodes.size, graph.nodes.size)("")

    graph.edges.foreach{ edge =>
      dist(nodeIndex(edge.source))(nodeIndex(edge.target)) = 1
      prev(nodeIndex(edge.source))(nodeIndex(edge.target)) = edge.source
    }
    graph.nodes.foreach { node =>
      dist(nodeIndex(node))(nodeIndex(node)) = 0
      prev(nodeIndex(node))(nodeIndex(node)) = node
    }

    for {
      k <- dist.indices
      i <- dist.indices
      j <- dist.indices
    } yield {
      if dist(i)(j) > dist(i)(k) + dist(k)(j) then
        dist(i)(j) = dist(i)(k) + dist(k)(j)
        prev(i)(j) = prev(k)(j)
    }

  }
