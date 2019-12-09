import scala.io.Source

object Problem04 extends App {

  val lines = Source.fromResource("04-input.txt").getLines().toList.sorted//.take(20)

  //println(lines.mkString("\n"))

  case class State(map: Map[Int, List[List[Int]]], currentGuard: Option[Int], currentSleepStart: Option[Int])

  val beginPattern = """\[\d{4}-\d{2}-\d{2} \d{2}:\d{2}\] Guard #(\d+) begins shift""".r
  val asleepPattern = """\[\d{4}-\d{2}-\d{2} \d{2}:(\d{2})\] falls asleep""".r
  val wakeupPattern = """\[\d{4}-\d{2}-\d{2} \d{2}:(\d{2})\] wakes up""".r

  val endState: State = lines.foldLeft(State(Map.empty, None, None)){
    case (state, line) => line match {
      case beginPattern(guardNr) if state.map.contains(guardNr.toInt) => State(
        state.map.updated(guardNr.toInt, Array.fill(60)(0).toList :: state.map(guardNr.toInt)),
        Some(guardNr.toInt),
        None
      )
      case beginPattern(guardNr) => State(
        state.map + (guardNr.toInt -> List(Array.fill(60)(0).toList)),
        Some(guardNr.toInt),
        None
      )
      case asleepPattern(minute) => State(
        state.map,
        state.currentGuard,
        Some(minute.toInt)
      )
      case wakeupPattern(minute) =>
        val guardNr = state.currentGuard.get
        val hour = state.map(guardNr).head.toArray
        (state.currentSleepStart.get until minute.toInt).foreach { i => hour(i) = 1 }
        State(
          state.map.updated(guardNr, hour.toList :: state.map(guardNr).tail),
          state.currentGuard,
          None
        )
    }
  }

  //println(endState)

  val guardNr = endState.map.mapValues(hours => hours.map(_.sum).sum).toList.maxBy(_._2)._1

  println(s"Guard $guardNr")

  val minute = endState.map(guardNr).reduce(_.zip(_).map { case (a, b) => a + b }).zipWithIndex.maxBy(_._1)._2

  println(s"Minute $minute")

  println(guardNr * minute)

  val best = endState.map.mapValues(_.reduce(_.zip(_).map { case (a, b) => a + b }).zipWithIndex.maxBy(_._1)).maxBy { case (guardNr, (maxAsleep, minute)) => maxAsleep}

  println(s"Guard ${best._1} minute ${best._2._2} maxAsleep ${best._2._1} answer: ${best._1 * best._2._2}")






}
