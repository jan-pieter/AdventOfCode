import optimus.algebra.{Const, Expression}

import scala.collection.mutable
import scala.io.Source

object Problem10 extends App:
  case class Machine(lightDiagram: Vector[Boolean], buttons: Vector[Vector[Int]], joltage: Vector[Int]):
    val diagrams: mutable.Map[Vector[Boolean], Long] = mutable.Map(lightDiagram -> 0L)
    def fewestPresses(currentDiagram: Vector[Boolean] = Vector.fill(lightDiagram.length)(false), seen: Set[Vector[Boolean]] = Set(Vector.fill(lightDiagram.length)(false))): Long = {
//      println(s"fewestPresses($currentDiagram, $seen)")
      diagrams.getOrElseUpdate(currentDiagram, {
        buttons.flatMap{ button =>
          val afterPressed = button.foldLeft(currentDiagram)((acc, index) => acc.updated(index, !acc(index)))
//          println(s"Trying $button: $afterPressed (seen: ${seen(afterPressed)})")
          if seen(afterPressed) then Vector.empty else
            Vector(fewestPresses(afterPressed, seen + afterPressed) + 1)
        }.minOption.getOrElse(Long.MaxValue - 1000000)
      })
    }

    def maxPresses(currentJoltage: Vector[Int]): Vector[Int] = {
      val joltageDiff = joltage.zip(currentJoltage).map((target, now) => target - now)
      buttons.map(button => button.map(pos => joltageDiff(pos)).min)
    }

    def correctJoltage(buttonPresses: Vector[Int]): Boolean = {
      val actual = buttonPresses.zip(buttons).foldLeft(Vector.fill(joltage.length)(0)) {
        case (acc, (presses, button)) => button.foldLeft(acc)((acc, index) => acc.updated(index, acc(index)+presses))
      }
      println(actual.mkString(","))
      println(joltage.mkString(","))
      actual == joltage
    }

    def solveWithMIP: Long = {
      import optimus.optimization.*
      import optimus.optimization.enums.SolverLib
      import optimus.optimization.model.MPIntVar
      given model: MPModel = MPModel(SolverLib.oJSolver)
      val presses = maxPresses(Vector.fill(joltage.length)(0))
      val mipVars = presses.zipWithIndex.map((p, index) => MPIntVar(s"button$index", 0 to p))

      minimize(mipVars.reduce(_ + _))
      subjectTo(joltage.zipWithIndex.map { (target, index) =>
        val vars: Vector[MPIntVar] = buttons.zipWithIndex.filter(_._1.contains(index)).map((_, buttonIndex) => mipVars(buttonIndex))
        vars.reduce[Expression](_ + _) := Const(target)
      }*)

      start()

      println(s"objective: $objectiveValue")
      println(mipVars.map(_.value).mkString(","))
      println(correctJoltage(mipVars.map(_.value.map(_.round.toInt).getOrElse(0))))
      val result = objectiveValue
      release()
      result.round
    }

  object Machine:
    private val regex = "^\\[([.#]+)] ((?:\\([0-9,]+\\) )+)\\{([0-9,]+)}$".r
    def fromString(s: String): Machine = s match {
      case regex(lights, buttons, joltage) => Machine(lights.toVector.map{
        case '#' => true
        case '.' => false
      }, buttons.trim.split(" ").toVector.map {
        case s"($ints)" => ints.split(",").toVector.map(_.toInt)
      }, joltage.split(",").toVector.map(_.toInt))
    }

//  val file = "10-test.txt"
  val file = "10-input.txt"
  val input = Source.fromResource(file).getLines().toVector.map(Machine.fromString)
//  println(input)
  val result = input.map(_.fewestPresses())
//  println(result.mkString(","))
  println(result.sum)

  println(input.map(_.solveWithMIP).sum)
