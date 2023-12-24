import scalax.collection.edges.labeled.WUnDiEdge
import scalax.collection.immutable.Graph

import java.io.{File, FileWriter}
import scala.io.Source

object Problem23 extends App:
  val input = Source.fromResource("23-input.txt").getLines().toVector
  val start = (0, 1)
  var target = (input.length-1, input.head.length-2)
  def isValid(y: Int, x: Int): Boolean = y >= 0 && y < input.length && x >= 0 && x < input.head.length

  def longestPath(position: (Int, Int), seen: Set[(Int, Int)]): Option[Vector[(Int, Int)]] = {
//    println(s"Longest path: $position, $seen")
    if position==target then Some(Vector(position)) else
      val newSeen = seen + position
      val neighbours: Vector[Option[Vector[(Int, Int)]]] = Vector(
        Option.when(isValid(position._1-1, position._2) && !seen(position._1-1, position._2) && input(position._1-1)(position._2) != '#' && Set('.','^').contains(input(position._1)(position._2)))(longestPath((position._1-1, position._2), newSeen)).flatten,
        Option.when(isValid(position._1+1, position._2) && !seen(position._1+1, position._2) && input(position._1+1)(position._2) != '#' && Set('.','v').contains(input(position._1)(position._2)))(longestPath((position._1+1, position._2), newSeen)).flatten,
        Option.when(isValid(position._1, position._2-1) && !seen(position._1, position._2-1) && input(position._1)(position._2-1) != '#' && Set('.','<').contains(input(position._1)(position._2)))(longestPath((position._1, position._2-1), newSeen)).flatten,
        Option.when(isValid(position._1, position._2+1) && !seen(position._1, position._2+1) && input(position._1)(position._2+1) != '#' && Set('.','>').contains(input(position._1)(position._2)))(longestPath((position._1, position._2+1), newSeen)).flatten
      )
      neighbours.maxByOption{
        case None => Int.MinValue
        case Some(path) => path.length
      }.flatten.map(_.prepended(position))
  }

  val lpath = longestPath(start, Set.empty)
  println(lpath.get.length-1)
  val graph: scalax.collection.mutable.Graph[String, WUnDiEdge[String]] = scalax.collection.mutable.Graph.empty
  graph.add(start.toString)
  def walk(position: (Int, Int), nextPosition: (Int, Int)): Unit = {
//    println(s"walk $position $nextPosition")
    var current = position
    var length = 0
    var neighbours = Vector(nextPosition)
    while (neighbours.size == 1 && neighbours.head != target) {
      val next = neighbours.head
      neighbours = Vector(
        Option.when(isValid(next._1 - 1, next._2) && input(next._1 - 1)(next._2) != '#')((next._1 - 1, next._2)),
        Option.when(isValid(next._1 + 1, next._2) && input(next._1 + 1)(next._2) != '#')((next._1 + 1, next._2)),
        Option.when(isValid(next._1, next._2 - 1) && input(next._1)(next._2 - 1) != '#')((next._1, next._2 - 1)),
        Option.when(isValid(next._1, next._2 + 1) && input(next._1)(next._2 + 1) != '#')((next._1, next._2 + 1))
      ).flatten.filterNot(_ == current)
      length = length + 1
      current = next
    }
    //println(s"junction at $current")
    if neighbours.size == 1 && neighbours.head == target then
//      println(s"Adding target $target")
      graph.add(target.toString)
      graph.add(WUnDiEdge(position.toString(), target.toString(), length+1))
    else if graph.contains(current.toString) then
      val edge = WUnDiEdge(position.toString(), current.toString(), length)
      graph.add(edge)
    else
      graph.add(current.toString)
      neighbours.foreach(next => graph.add(WUnDiEdge(position.toString(), current.toString(), length)))
      neighbours.foreach(next => walk(current, next))
  }
  walk(start, (start._1+1, start._2))
  println(s"${graph.nodes.size} nodes and ${graph.edges.size} edges")
//  println(s"Nodes: ${graph.nodes.mkString(",")}")
//  println(s"Edges: ${graph.edges.mkString(",")}")

  def longestPath2(source: String, seen: Set[String]): Option[Vector[(String, Int)]] = {
//    println(s"Longest path2: $source, $seen")
    val newSeen = seen + source
    if source == target.toString() then Some(Vector.empty)
    else
      graph.get(source).edges.flatMap { edge =>
//        println(s"Edge $edge")
        val other = if edge.source == source then edge.target else edge.source
        if seen(other) then None
        else longestPath2(other, newSeen).map(_.prepended((other, edge.weight.toInt)))
      }.maxByOption(_.map(_._2).sum)
  }
  val startS = System.nanoTime()
  val longest = longestPath2(start.toString(), Set.empty).get
  val endS = System.nanoTime()
  println(s"Longest path of ${longest.map(_._2).sum}")
  println(s"Took ${endS-startS} ns")

  import scalax.collection.io.dot.*
  import implicits.*
  val root = DotRootGraph(directed = false, id = Some(Id("Day23")))
  def edgeTransformer(innerEdge: Graph[String, WUnDiEdge[String]]#GraphInnerEdge): Option[(DotGraph, DotEdgeStmt)] = {
    val edge = innerEdge.outer
    val label = edge.weight.toInt
    Some(
      root,
      DotEdgeStmt(
        NodeId(edge.source),
        NodeId(edge.target),
        List(DotAttr(Id("label"), Id(label)))
      )
    )
  }
  val imGraph = Graph.from[String, WUnDiEdge[String]](graph.nodes.map(_.toString()), graph.edges.map(edge => WUnDiEdge(edge.source, edge.target, edge.weight)))
  val dot = imGraph.toDot(root, edgeTransformer)
  val fileWriter = new FileWriter(new File("/tmp/graph.dot"))
  fileWriter.write(dot)
  fileWriter.close()
