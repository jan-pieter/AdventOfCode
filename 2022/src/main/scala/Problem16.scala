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

  case class State3(me: (String, Int), elephant: (String, Int), minutesLeft: Long, flow: Long, closedValves: Set[String]) {
    lazy val nextStates: Vector[State3] = {
      // Both choose new target?
      if (me._2 == 0 && !closedValves.contains(me._1)) {
        // I choose new target
        val meOptions = closedValves.filter(valve => stepsTo(me._1, valve) < minutesLeft).filter(_ != elephant._1)
        if (meOptions.isEmpty) {
          // Nowhere to go, move forward
          if (elephant._2 > 0) {
            Vector(copy(elephant = elephant._1 -> 0, minutesLeft = minutesLeft - elephant._2, 0))
          } else if (elephant._2 == 0 && closedValves.contains(elephant._1)) {
            Vector(copy(minutesLeft = minutesLeft - 1, flow = input(elephant._1).flow, closedValves = closedValves - elephant._1))
          } else Vector.empty
        } else {
          meOptions.toVector.map(valve => copy(me = valve -> stepsTo(me._1, valve), flow = 0))
        }
      } else if (elephant._2 == 0 && !closedValves.contains(elephant._1)) {
        // Elephant chooses new target
        val elephantOptions = closedValves.filter(valve => stepsTo(elephant._1, valve) < minutesLeft).filter(_ != me._1) // Include when it is last?
        if (elephantOptions.isEmpty) {
          // Nowhere to go, move forward
          if (me._2 > 0) {
            Vector(copy(me = me._1 -> 0, minutesLeft = minutesLeft - me._2, 0))
          } else if (me._2 == 0 && closedValves.contains(me._1)) {
            Vector(copy(minutesLeft = minutesLeft - 1, flow = input(me._1).flow, closedValves = closedValves - me._1))
          } else Vector.empty
        } else {
          elephantOptions.toVector.map(valve => copy(elephant = valve -> stepsTo(elephant._1, valve), flow = 0))
        }
      } else if (me._2 == 0 && elephant._2 == 0 && closedValves.contains(me._1) && closedValves.contains(elephant._1)) {
        // Both open valve
        Vector(copy(minutesLeft = minutesLeft - 1, flow = Set(me._1, elephant._1).map(room => input(room).flow).sum, closedValves = closedValves - me._1 - elephant._1))
      } else if (me._2 == 0 && closedValves.contains(me._1)) {
        // I open valve
        Vector(copy(elephant = elephant._1 -> (elephant._2 - 1), minutesLeft = minutesLeft - 1, flow = input(me._1).flow, closedValves = closedValves - me._1))
      } else if (elephant._2 == 0 && closedValves.contains(elephant._1)) {
        // Elephant opens valve
        Vector(copy(me = me._1 -> (me._2 - 1), minutesLeft = minutesLeft - 1, flow = input(elephant._1).flow, closedValves = closedValves - elephant._1))
      } else {
        // Move
        val steps = Math.min(me._2, elephant._2)
        Vector(copy(me = me._1 -> (me._2 - steps), elephant = elephant._1 -> (elephant._2 - steps), minutesLeft = minutesLeft - steps, 0L))
      }
    }

    override def toString: String = //if (me._1 == "AA" && elephant._1 == "AA") {
//      s"State($me, $elephant, $minutesLeft, $flow, $closedValves)\n" + nextStates.filter(state => state.me._1 == "JJ").map(_.toString).mkString
//    } else if (flow == 35) {
//      s"State($me, $elephant, $minutesLeft, $flow, $closedValves)\n" + nextStates.filter(state => state.me._1 == "CC").map(_.toString).mkString
//    } else {
      s"State($me, $elephant, $minutesLeft, $flow, $closedValves)\n" + nextStates.maxByOption(_.score).map(_.toString).getOrElse("END")
//    }

    def score: Long = flow * minutesLeft + nextStates.map(_.score).maxOption.getOrElse(0L)
  }

  val solution3 = State3(("AA", 0), ("AA", 0), 26, 0L, usefulValves)
  println(solution3)
  println(solution3.score)
  sys.exit(1)

  case class State2(room: (String, Int), elephant: (String, Int), minutesLeft: Long, closedValves: Set[String], openFlow: Long, nextStates: Vector[State2]) {
    def score: Long = {
      openFlow * minutesLeft + nextStates.map(_.score).maxOption.getOrElse(0L)
    }
//    println(this)
//    printed += 1
//    if (printed >= 100) sys.exit(1)
    override def toString: String = if (room._1 == "AA" && elephant._1 == "AA") {
      s"State(room: $room, elephant: $elephant minutesLeft: $minutesLeft, closedValves: $closedValves)\n" + nextStates.filter(state => state.room._1 == "JJ" && state.elephant._1 == "DD").map(_.toString).mkString
    }  else {
      s"State(room: $room, elephant: $elephant minutesLeft: $minutesLeft, closedValves: $closedValves)\n" + nextStates.maxByOption(_.score).map(_.toString).getOrElse("END")
    }

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
      val r = (state.closedValves - state.room._1 - state.elephant._1).toVector.flatMap { valve =>
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
      if (r.nonEmpty) {
        r
      } else if (state.room._2 > 0 && state.room._2 == state.elephant._2){
        Vector(state.copy(room = state.room._1 -> 0, elephant = state.elephant._1 -> 0, minutesLeft = state.minutesLeft - state.room._2, closedValves = state.closedValves - state.room._1 - state.elephant._1, input(state.room._1).flow + input(state.elephant._1).flow, Vector.empty))
      } else if (state.room._2 > 0 && state.elephant._2 > 0 && state.room._2 > state.elephant._2){
        val s = state.copy(room = state.room._1 -> (state.room._2 - state.elephant._2), elephant = state.elephant._1 -> 0, minutesLeft = state.minutesLeft - state.elephant._2, closedValves = state.closedValves - state.elephant._1, input(state.elephant._1).flow, Vector.empty)
        Vector(s.copy(nextStates = nextStep2(s)))
      } else if (state.room._2 > 0 && state.room._2 < state.elephant._2) {
        val s = state.copy(room = state.room._1 -> 0, elephant = state.elephant._1 -> (state.elephant._2 - state.room._2), minutesLeft = state.minutesLeft - state.room._2, closedValves = state.closedValves - state.room._1, input(state.room._1).flow, Vector.empty)
        Vector(s.copy(nextStates = nextStep2(s)))
      } else if (state.room._2 > 0) {
        Vector(state.copy(room = state.room._1 -> 0, minutesLeft = state.minutesLeft - state.room._2, closedValves = state.closedValves - state.room._1, input(state.room._1).flow, Vector.empty))
      } else if (state.elephant._2 > 0) {
        Vector(state.copy(elephant = state.elephant._1 -> 0, minutesLeft = state.minutesLeft - state.elephant._2, closedValves = state.closedValves - state.elephant._1, input(state.elephant._1).flow, Vector.empty))
      } else {
        r
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
