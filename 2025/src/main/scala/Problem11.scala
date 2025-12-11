import scalax.collection.edges.DiEdge
import scalax.collection.mutable.Graph

import scala.collection.mutable
import scala.io.Source

object Problem11 extends App:
//  val file = "11-test.txt"
//  val file = "11-test2.txt"
  val file = "11-input.txt"
  val input = Source.fromResource(file).getLines().toVector.flatMap {
    case s"$from: $to" => to.split(" ").toVector.map(from -> _)
  }
//  println(input)

  val graph = Graph.empty[String, DiEdge[String]]
  input.flatMap((from, to) => Vector(from, to)).toSet.foreach(node => graph.add(node))
  input.foreach((from, to) => graph.add(DiEdge(from, to)))

  print(graph.isAcyclic)

  println(countPaths(graph, "you", "out"))

  val svr2dac = countPaths(graph, "svr", "dac")
  val svr2fft = countPaths(graph, "svr", "fft")
  val dac2fft = countPaths(graph, "dac", "fft")
  val fft2dac = countPaths(graph, "fft", "dac")
  val dac2out = countPaths(graph, "dac", "out")
  val fft2out = countPaths(graph, "fft", "out")

  println(s"svr2dac=$svr2dac svr2fft=$svr2fft dac2fft=$dac2fft fft2dac=$fft2dac dac2out=$dac2out fft2out=$fft2out")
  println(svr2fft * fft2dac * dac2out + svr2dac * dac2fft * fft2out)

  def countPaths(g: Graph[String, DiEdge[String]], source: String, target: String): BigInt = {
    val topo = g.topologicalSort.getOrElse(???).toVector

    val counts = mutable.Map[g.NodeT, BigInt]().withDefaultValue(0)
    counts(g.get(source)) = 1

    topo.foreach { n =>
      val c = counts(n)
      if c > 0 then
        n.diSuccessors.foreach { succ =>
          counts(succ) = counts(succ) + c
        }
    }

    counts(g.get(target))
  }
