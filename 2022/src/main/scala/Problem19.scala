import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._
import scala.io.Source

object Problem19 extends App:
  case class Blueprint(id: Int, oreRobotOreCost: Int, clayRobotOreCost: Int, obsidianRobotOreCost: Int, obsidianRobotClayCost: Int, geodeRobotOreCost: Int, geodeRobotObsidianCost: Int) {
    case class State(minutesLeft: Int, ore: Int, clay: Int, obsidian: Int, geode: Int, oreRobots: Int, clayRobots: Int, obsidianRobots: Int, geodeRobots: Int) {
      def upperBound: Long = {
        val canBuildOreRobot = ore >= oreRobotOreCost
        val canBuildObsidianRobot = ore >= obsidianRobotOreCost && clay >= obsidianRobotClayCost
        val canBuildGeodeRobot = ore >= geodeRobotOreCost && obsidian >= geodeRobotObsidianCost
        val canBuildGeodeRobotNextStep = (ore + oreRobots + (if (canBuildOreRobot) 1 else 0)) >= geodeRobotOreCost && (obsidian + obsidianRobots + (if (canBuildObsidianRobot) 1 else 0)) >= geodeRobotObsidianCost
        geode + geodeRobots * minutesLeft + (0 until (if (canBuildGeodeRobot) minutesLeft else if (canBuildGeodeRobotNextStep) minutesLeft - 1 else minutesLeft - 2)).sum
      }
    }
    def quality: Long = {
      val q = nextStep(State(24, 0, 0, 0, 0, 1 ,0, 0, 0), 0, false).geode * id
      println(s"Quality $id: $q")
      q
    }

    def geodes: Long = {
      val q = nextStep(State(32, 0, 0, 0, 0, 1, 0, 0, 0), 0, false).geode.toLong
      println(s"Geodes $id: $q")
      q
    }

    def nextStep(state: State, bestSoFar: Long, greedy: Boolean = false): State = {
      if (state.minutesLeft == 0) {
        //        if (state.geode > 0)
        //          println(s"Reached endstate with ${state.geode} geodes.")
        state
      } else if (state.upperBound <= bestSoFar) {
//        println("pruned")
        state
      } else {
        var states: Vector[State] = Vector.empty
        val newMinutes = state.minutesLeft - 1
        val newOre = state.ore + state.oreRobots
        val newClay = state.clay + state.clayRobots
        val newObsidian = state.obsidian + state.obsidianRobots
        val newGeode = state.geode + state.geodeRobots
        val maxOreNeeded = List(clayRobotOreCost, obsidianRobotOreCost, geodeRobotOreCost).max
        val oreStillUseful = newOre < maxOreNeeded * state.minutesLeft && state.oreRobots < maxOreNeeded
        val clayStillUseful = newClay + state.clayRobots * state.minutesLeft + (0 until state.minutesLeft).sum > obsidianRobotClayCost
        val obsidianStillUseful = newObsidian + state.obsidianRobots * state.minutesLeft + (0 until state.minutesLeft).sum > geodeRobotObsidianCost
        if (state.ore >= geodeRobotOreCost && state.obsidian >= geodeRobotObsidianCost) {
          states = states :+ state.copy(newMinutes, newOre - geodeRobotOreCost, newClay, newObsidian - geodeRobotObsidianCost, newGeode, geodeRobots = state.geodeRobots + 1)
        }
        if (state.ore >= obsidianRobotOreCost && state.clay >= obsidianRobotClayCost && obsidianStillUseful) {
          states = states :+ state.copy(newMinutes, newOre - obsidianRobotOreCost, newClay - obsidianRobotClayCost, newObsidian, newGeode, obsidianRobots = state.obsidianRobots + 1)
        }
        if (state.ore >= clayRobotOreCost && clayStillUseful) {
          states = states :+ state.copy(newMinutes, newOre - clayRobotOreCost, newClay, newObsidian, newGeode, clayRobots = state.clayRobots + 1)
        }
        if (state.ore >= oreRobotOreCost && oreStillUseful) {
          states = states :+ state.copy(newMinutes, newOre - oreRobotOreCost, newClay, newObsidian, newGeode, oreRobots = state.oreRobots + 1)
        }
        states = states :+ state.copy(newMinutes, newOre, newClay, newObsidian, newGeode)
        if (greedy)
          nextStep(states.head, 0, true)
        else
          states.foldLeft(nextStep(states.head, bestSoFar, true)){ (bestState, newState) =>
            val nextState = nextStep(newState, bestState.geode, false)
            if (nextState.geode >= bestState.geode) nextState else bestState
          }
      }
    }
  }

  val input = Source.fromResource("19-input.txt").getLines().toVector.par.map {
    case s"Blueprint $id: Each ore robot costs $oreRobotOreCost ore. Each clay robot costs $clayRobotOreCost ore. Each obsidian robot costs $obsidianRobotOreCost ore and $obsidianRobotClayCost clay. Each geode robot costs $geodeRobotOreCost ore and $geodeRobotObsidianCost obsidian." =>
      Blueprint(id.toInt, oreRobotOreCost.toInt, clayRobotOreCost.toInt, obsidianRobotOreCost.toInt, obsidianRobotClayCost.toInt, geodeRobotOreCost.toInt, geodeRobotObsidianCost.toInt)
  }

//  val start = System.nanoTime()
//  val solution1 = input.map(_.quality).sum
//  val duration = (System.nanoTime() - start) / 1000000000L
//  println(s"Duration part 1: $duration seconds")
//  println(s"Solution 1: $solution1")

  val start2 = System.nanoTime()
  val solution2 = input.take(3).map(_.geodes).product
  val duration2 = (System.nanoTime() - start2) / 1000000000L
  println(s"Duration part 2: $duration2 seconds")
  println(s"Solution 2: $solution2")
