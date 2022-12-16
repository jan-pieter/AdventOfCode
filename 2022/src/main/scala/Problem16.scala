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
    length + 1
  })

  def nextStep(state: State): Vector[State] = {
    state.closedValves.toVector.filter(valve => stepsTo(state.room, valve) <= state.minutesLeft).map { valve =>
      val s = state.copy(room = valve, minutesLeft = state.minutesLeft - stepsTo(state.room, valve), closedValves = state.closedValves - valve, input(valve).flow)
      s.copy(nextStates = nextStep(s))
    }
  }
  val start = State("AA", 30, usefulValves, 0L, Vector.empty)
  val solution1 = start.copy(nextStates = nextStep(start))
  println("Determined solution")
  println(solution1.score)
//  sys.exit(1)

//  case class State3()

  case class State2(room: (String, Int), elephant: (String, Int), minutesLeft: Long, closedValves: Set[String], openFlow: Long, nextStates: Vector[State2]) {
    def score: Long = {
      openFlow * minutesLeft + nextStates.map(_.score).maxOption.getOrElse(0L)
    }
//    println(this)
//    printed += 1
//    if (printed >= 100) sys.exit(1)
    override def toString: String = s"State(room: $room, elephant: $elephant minutesLeft: $minutesLeft, closedValves: $closedValves)\n" + nextStates.maxByOption(_.score).map(_.toString).getOrElse("END")
  }

  def nextStep2(state: State2): Vector[State2] = {
    if (state.room._2 == 0 && state.elephant._2 == 0) {
      // both take a step
      val meOptions = state.closedValves.filter(valve => stepsTo(state.room._1, valve) <= state.minutesLeft)
      val elephantOptions = state.closedValves.filter(valve => stepsTo(state.elephant._1, valve) <= state.minutesLeft)
      if (elephantOptions.isEmpty && meOptions.isEmpty) {
        Vector.empty
      } else if (elephantOptions.isEmpty || (meOptions.size == 1 && meOptions == elephantOptions)) {
        val nextMe = meOptions.head
        // Only I can take a step
        Vector(state.copy(room = nextMe -> 0, minutesLeft = state.minutesLeft - stepsTo(state.room._1, nextMe), closedValves = state.closedValves - nextMe, input(nextMe).flow, nextStates = Vector.empty))
      } else if (meOptions.isEmpty) {
        // Only Elephant can take a step
        val nextElephant = elephantOptions.head
        Vector(state.copy(elephant = nextElephant -> 0, minutesLeft = state.minutesLeft - stepsTo(state.elephant._1, nextElephant), closedValves = state.closedValves - nextElephant, input(nextElephant).flow, nextStates = Vector.empty))
      } else {
        (for {
          nextMe <- meOptions
          nextElephant <- elephantOptions
          if nextMe != nextElephant
        } yield {
          val mySteps = stepsTo(state.room._1, nextMe)
          val elephantSteps = stepsTo(state.elephant._1, nextElephant)
          Option.when(mySteps <= state.minutesLeft && elephantSteps <= state.minutesLeft)(
            if (mySteps == elephantSteps) {
              // Both at target
              val s = state.copy(room = nextMe -> 0, elephant = nextElephant -> 0, minutesLeft = state.minutesLeft - mySteps, closedValves = state.closedValves - nextMe - nextElephant, input(nextMe).flow + input(nextElephant).flow)
              s.copy(nextStates = nextStep2(s))
            } else if (mySteps < elephantSteps) {
              // I'm first at my target
              val s = state.copy(room = nextMe -> 0, elephant = nextElephant -> (elephantSteps - mySteps), minutesLeft = state.minutesLeft - mySteps, closedValves = state.closedValves - nextMe, input(nextMe).flow)
              s.copy(nextStates = nextStep2(s))
            } else {
              // Elephant first at target
              val s = state.copy(room = nextMe -> (mySteps - elephantSteps), elephant = nextElephant -> 0, minutesLeft = state.minutesLeft - elephantSteps, closedValves = state.closedValves - nextElephant, input(nextElephant).flow)
              s.copy(nextStates = nextStep2(s))
            }
          )
        }).flatten.toVector
      }
    } else {
      (state.closedValves - state.room._1 - state.elephant._1).toVector.flatMap { valve =>
        if (state.room._2 == 0) { // I need to pick a new target
          val mySteps = stepsTo(state.room._1, valve)
          Option.when(mySteps <= state.minutesLeft)(
            if (mySteps == state.elephant._2) {
              // Both at target
              val s = state.copy(room = valve -> 0, elephant = state.elephant._1 -> 0, minutesLeft = state.minutesLeft - mySteps, closedValves = state.closedValves - valve - state.elephant._1, input(valve).flow + input(state.elephant._1).flow)
              s.copy(nextStates = nextStep2(s))
            } else if (mySteps < state.elephant._2) {
              // I'm first at my target
              val s = state.copy(room = valve -> 0, elephant = state.elephant._1 -> (state.elephant._2 - mySteps), minutesLeft = state.minutesLeft - mySteps, closedValves = state.closedValves - valve, input(valve).flow)
              s.copy(nextStates = nextStep2(s))
            } else {
              // Elephant first at target
              val s = state.copy(room = valve -> (mySteps - state.elephant._2), elephant = state.elephant._1 -> 0, minutesLeft = state.minutesLeft - state.elephant._2, closedValves = state.closedValves - state.elephant._1, input(state.elephant._1).flow)
              s.copy(nextStates = nextStep2(s))
            }
          )
        } else { // Elephant needs to pick a new target
          require(state.elephant._2 == 0, s"Elephant ${state.elephant._2} != 0")
          val elephantSteps = stepsTo(state.elephant._1, valve)
          Option.when(elephantSteps <= state.minutesLeft)(
            if (elephantSteps == state.room._2) {
              // Both at target
              val s = state.copy(room = state.room._1 -> 0, elephant = valve -> 0, minutesLeft = state.minutesLeft - elephantSteps, closedValves = state.closedValves - state.room._1 - valve, input(state.room._1).flow + input(valve).flow)
              s.copy(nextStates = nextStep2(s))
            } else if (elephantSteps > state.room._2) {
              // I'm first at my target
              val s = state.copy(room = state.room._1 -> 0, elephant = valve -> (elephantSteps - state.room._2), minutesLeft = state.minutesLeft - state.room._2, closedValves = state.closedValves - state.room._1, input(state.room._1).flow)
              s.copy(nextStates = nextStep2(s))
            } else {
              // Elephant first at target
              val s = state.copy(room = state.room._1 -> (elephantSteps - state.elephant._2), elephant = valve -> 0, minutesLeft = state.minutesLeft - elephantSteps, closedValves = state.closedValves - valve, input(valve).flow)
              s.copy(nextStates = nextStep2(s))
            }
          )
        }
      }
    }
  }

  val start2 = State2(("AA", 0), ("AA", 0), 26, usefulValves, 0L, Vector.empty)
  val solution2 = start2.copy(nextStates = nextStep2(start2))
  println("Determined solution")
  println(solution2)
  println(solution2.score)

//  val solution2 = input.map(_.sum).sorted.takeRight(3).sum
//  println(solution2)
