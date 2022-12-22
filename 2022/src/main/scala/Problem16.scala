import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef.EdgeAssoc
import scalax.collection.mutable.Graph

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Problem16 extends App:
  case class Room(name: String, flow: Long, tunnels: Vector[String])
  val input = Source.fromResource("16-input.txt").getLines().toVector.map {
    case s"Valve $name has flow rate=$flow; tunnel leads to valve $tunnel" => name -> Room(name, flow.toLong, Vector(tunnel))
    case s"Valve $name has flow rate=$flow; tunnels lead to valves $tunnels" => name -> Room(name, flow.toLong, tunnels.split(", ").toVector)
  }.toMap

  case class State(room: String, minutesLeft: Long, closedValves: Set[String], openFlow: Long, nextStates: Vector[State]) {
    def score: Long = openFlow * minutesLeft + nextStates.map(_.score).maxOption.getOrElse(0L)
  }

  val usefulValves = input.values.filter(_.flow > 0).map(_.name).toSet

  val graph: Graph[String, UnDiEdge] = Graph()
  input.values.foreach(room => graph.add(room.name))
  input.values.foreach(room => room.tunnels.foreach(to => graph.add(room.name ~ to)))

  println(graph.toString())

  val cache: mutable.Map[(String, String), Int] = mutable.Map.empty
  def stepsTo(from: String, to: String): Int = cache.getOrElseUpdate((from, to), {
    val length = graph.get(from).shortestPathTo(graph.get(to)).get.nodes.size - 1
//    println(s"$from $to $length")
    length
  })

  def nextStep(state: State): Vector[State] = {
    state.closedValves.toVector.filter(valve => stepsTo(state.room, valve) < state.minutesLeft).map { valve =>
      val s = state.copy(room = valve, minutesLeft = state.minutesLeft - (stepsTo(state.room, valve) + 1), closedValves = state.closedValves - valve, input(valve).flow)
      s.copy(nextStates = nextStep(s))
    }
  }
  val start = State("AA", 30, usefulValves, 0L, Vector.empty)
  val solution1 = start.copy(nextStates = nextStep(start))
  println("Determined solution")
  println(solution1.score)
//  sys.exit(1)

  val nodeSplits = usefulValves.toVector.combinations(usefulValves.size / 2).toVector.map(myNodes => (myNodes.toSet, usefulValves.diff(myNodes.toSet)))
  val solutions = nodeSplits.map{ (myValves, elephantValves) =>
    val myStart = State("AA", 26, myValves, 0L, Vector.empty)
    val mySolution = myStart.copy(nextStates = nextStep(myStart))
    val elephantStart = State("AA", 26, elephantValves, 0L, Vector.empty)
    val elephantSolution = elephantStart.copy(nextStates = nextStep(elephantStart))
    val score = mySolution.score + elephantSolution.score
//    println(s"Score: $score")
    score
  }
  println("Determined solution")
  println(solutions.max)
//  nodeSplits.foreach(println(_))
