import scalax.collection.edges.UnDiEdge
import scalax.collection.mutable.Graph

import scala.io.Source

object Problem18 extends App:
//  val (file, firstBytes, gridSize) = ("18-test.txt", 12, 7)
  val (file, firstBytes, gridSize) = ("18-input.txt", 1024, 71)
  val input = Source.fromResource(file).getLines().toVector
  val bytes = input.map {
    case s"$x,$y" => (y.toInt, x.toInt)
  }

  def shortestPathLength(firstBytes: Int): Option[Int] = {
    val firstByteSet = bytes.take(firstBytes).toSet

    val graph = Graph.empty[(Int, Int), UnDiEdge[(Int, Int)]]
    for {
      y <- 0 until gridSize
      x <- 0 until gridSize
      if !firstByteSet.contains(y -> x)
    } yield {
      graph.add(y -> x)
      if (y < gridSize - 1 && !firstByteSet.contains((y + 1) -> x))
        graph.add(UnDiEdge(y -> x, (y + 1) -> x))
      if (x < gridSize - 1 && !firstByteSet.contains(y -> (x + 1)))
        graph.add(UnDiEdge(y -> x, y -> (x + 1)))
    }
    graph.get((0, 0)).shortestPathTo(graph.get((gridSize - 1, gridSize - 1))).map { path =>
      path.edges.size
    }
  }
  println(shortestPathLength(firstBytes).get)

  val byteIndex: Int = (firstBytes to bytes.size).find(shortestPathLength(_).isEmpty).get
  val result2 = bytes(byteIndex-1)
  println(s"${result2._2},${result2._1}")



