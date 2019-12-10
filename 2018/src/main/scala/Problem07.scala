import scala.collection.mutable
import scala.io.Source

object Problem07 extends App {

  val lines = Source.fromResource("07-input.txt").getLines().toList

  val dependencies = lines.map { line =>
    val splitted = line.split(" ")
    splitted(1) -> splitted(7)
  }

  val dependenciesToSatisfy: mutable.Set[(String, String)] = mutable.Set(dependencies :_*)

  val toExecute: mutable.SortedSet[String] = mutable.SortedSet(dependencies.map(_._1).filterNot(dependencies.map(_._2).contains) :_*)

  val executeOrder: mutable.Queue[String] = mutable.Queue.empty

  while(toExecute.nonEmpty) {
    val task = toExecute.head
    executeOrder.enqueue(task)
    toExecute.remove(task)
    val possibleAdditions = dependenciesToSatisfy.filter(_._1 == task)
    possibleAdditions.map(dependenciesToSatisfy.remove)
    possibleAdditions.map(_._2).diff(dependenciesToSatisfy.map(_._2)).map(toExecute.add)
  }

  println(executeOrder.mkString)

}
