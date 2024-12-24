import scalax.collection.edges.UnDiEdge
import scalax.collection.mutable.Graph

import java.io.{File, FileWriter}
import scala.io.Source

object Problem24 extends App:
//  val file = "24-test.txt"
  val file = "24-input.txt"
  val input = Source.fromResource(file).getLines().toVector
  sealed trait Wire
  case class Literal(value: Boolean) extends Wire
  case class And(a: String, b: String) extends Wire
  case class Xor(a: String, b: String) extends Wire
  case class Or(a: String, b: String) extends Wire
  val wires = input.filter(_.nonEmpty).map{
    case s"$name: 1" => name -> Literal(true)
    case s"$name: 0" => name -> Literal(false)
    case s"$a AND $b -> $c" => c -> And(a, b)
    case s"$a XOR $b -> $c" => c -> Xor(a, b)
    case s"$a OR $b -> $c" => c -> Or(a, b)
  }.toMap

  def value(wire: String, wires: Map[String, Wire]): Boolean = {
    val cache = collection.mutable.Map.empty[String, Boolean]

    def value(wire: String): Boolean = cache.getOrElseUpdate(wire,
      wires(wire) match {
        case Literal(value) => value
        case And(a, b) => value(a) && value(b)
        case Xor(a, b) => value(a) ^ value(b)
        case Or(a, b) => value(a) || value(b)
      }
    )
    value(wire)
  }

  def hasCycle(wire: String, wires: Map[String, Wire], visited: Set[String] = Set.empty): Boolean = {
    visited(wire) || (wires(wire) match {
      case Literal(_) => false
      case And(a, b) => hasCycle(a, wires, visited + wire) || hasCycle(b, wires, visited + wire)
      case Xor(a, b) => hasCycle(a, wires, visited + wire) || hasCycle(b, wires, visited + wire)
      case Or(a, b) => hasCycle(a, wires, visited + wire) || hasCycle(b, wires, visited + wire)
    })
  }
  def hasCycle(wires: Map[String, Wire]): Boolean = wires.filter(_._1.startsWith("z")).exists((name, _) => hasCycle(name, wires))

  val result = wires.filter(_._1.startsWith("z")).map((name, _) => name -> value(name, wires)).toVector.sorted.reverse.map(value => if value._2 then "1" else "0").mkString
  println(BigInt(result, 2))

  val numberLength = wires.count(_._1.startsWith("x"))

  def wrongAnswers(wires: Map[String, Wire]): Set[(String, String)] = (0 until numberLength).flatMap { i =>
    val iString = f"$i%02d"
    val iplusString = f"${i + 1}%02d"
    val zeroWires = wires.map((name, value) => if name.startsWith("x") || name.startsWith("y") then
      name -> Literal(false)
    else name -> value)
    val xTrueWires = zeroWires.updated(s"x$iString", Literal(true))
    val yTrueWires = zeroWires.updated(s"y$iString", Literal(true))
    (if !value(s"z$iString", xTrueWires) then Set(s"x$iString" -> s"z$iString") else Set.empty) ++
      (if !value(s"z$iString", yTrueWires) then Set(s"y$iString" -> s"z$iString") else Set.empty)
  }.toSet

  println(wrongAnswers(wires)) // Which outputs are wrong
  createAndWriteGraph(wires) // Visually inspect which need to be swapped
  println(wrongAnswers(wires
    .updated("wbw", wires("wgb")).updated("wgb", wires("wbw"))
    .updated("z09", wires("gwh")).updated("gwh", wires("z09"))
    .updated("z21", wires("rcb")).updated("rcb", wires("z21"))
    .updated("z39", wires("jct")).updated("jct", wires("z39"))
  )) // Verify that the swaps are correct
  val result2 = Vector("wbw", "wgb", "z09", "gwh", "z21", "rcb", "z39", "jct").sorted.mkString(",")
  println(result2)

  def createAndWriteGraph(wires: Map[String, Wire]): Unit = {
    val graph = Graph.empty[String, UnDiEdge[String]]
    wires.foreach((name, _) => graph.add(name))
    wires.foreach {
      case (name, And(a, b)) =>
        graph.add(UnDiEdge(name, a))
        graph.add(UnDiEdge(name, b))
      case (name, Or(a, b)) =>
        graph.add(UnDiEdge(name, a))
        graph.add(UnDiEdge(name, b))
      case (name, Xor(a, b)) =>
        graph.add(UnDiEdge(name, a))
        graph.add(UnDiEdge(name, b))
      case _ =>
    }

    import scalax.collection.io.dot.*
    import implicits.*

    val root = DotRootGraph(directed = false, id = Some(Id("Day24")))

    def edgeTransformer(innerEdge: scalax.collection.immutable.Graph[String, UnDiEdge[String]]#GraphInnerEdge): Option[(DotGraph, DotEdgeStmt)] = {
      val edge = innerEdge.outer
      //val label = edge.weight.toInt
      Some(
        root,
        DotEdgeStmt(
          NodeId(edge.source),
          NodeId(edge.target),
          List() //List(DotAttr(Id("label"), Id(label)))
        )
      )
    }

    def nodeToColor(node: String): String = wires(node) match {
      case Literal(_) => "black"
      case And(_, _) => "red"
      case Or(_, _) => "blue"
      case Xor(_, _) => "green"
    }

    def nodeTransformer(innerNode: scalax.collection.immutable.Graph[String, UnDiEdge[String]]#GraphInnerNode): Option[(DotGraph, DotNodeStmt)] =
      Some(root, DotNodeStmt(innerNode.toString, List(DotAttr(Id("color"), Id(nodeToColor(innerNode.toString))))))

    val imGraph = scalax.collection.immutable.Graph.from[String, UnDiEdge[String]](graph.nodes.map(_.toString()), graph.edges.map(edge => UnDiEdge(edge.source, edge.target)))
    val dot = imGraph.toDot(root, edgeTransformer, cNodeTransformer = Some(nodeTransformer))
    val fileWriter = new FileWriter(new File("/tmp/graph.dot"))
    fileWriter.write(dot)
    fileWriter.close()
  }
