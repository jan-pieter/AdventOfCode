import scala.collection.mutable
import scala.io.Source

object Problem07 extends App {

  val lines = Source.fromResource("07-input.txt").getLines().toList

  val dependencies = lines.map { line =>
    val splitted = line.split(" ")
    splitted(1) -> splitted(7)
  }

  val dependenciesToSatisfy: mutable.Set[(String, String)] = mutable.Set(dependencies*)

  val toExecute: mutable.SortedSet[String] = mutable.SortedSet(dependencies.map(_._1).filterNot(dependencies.map(_._2).contains)*)

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

  var seconds = 0L
  val delay = 60
  val workers = 5

  def executeTime(step: String): Int = step(0) - 'A' + 1 + delay
  def canBePickedUp(step: String): Boolean = dependencies.filter(_._2 == step).forall(dependency => !executeOrder.contains(dependency._1))
  val workerTasks: Array[Option[(String, Int)]] = Array.fill(workers)(None)

  def newTask(): Option[(String, Int)] = executeOrder.find(task => canBePickedUp(task) && !workerTasks.flatMap(_.map(_._1)).contains(task)).map(task => task -> executeTime(task))

  while(executeOrder.nonEmpty) {
    seconds += 1
    (0 until workers).foreach { worker =>
      workerTasks.update(worker, workerTasks(worker) match {
        case Some(task) if task._2 == 1 =>
          executeOrder.dequeueFirst(_ == task._1)
          newTask()
        case Some(task) =>
          Some(task._1 -> (task._2 - 1))
        case None =>
          newTask()
      })
    }
    //println(s"$seconds ${workerTasks(0).getOrElse(".")} ${workerTasks(1).getOrElse(".")}")
  }
  println(seconds - 1)


}
