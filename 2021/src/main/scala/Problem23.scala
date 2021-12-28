import scala.collection.mutable

object Problem23 extends App {

  def cost(pod: Char): Long = pod match {
    case 'A' => 1
    case 'B' => 10
    case 'C' => 100
    case 'D' => 1000
  }

  case class World(hall: String, room1: String, room2: String, room3: String, room4: String, energy: Long, previous: Option[World]){
    val complete: Boolean = room1 == "AA" && room2 == "BB" && room3 == "CC" && room4 == "DD"
    def print(): Unit = {
      println(s"#############   $energy")
      println(s"#$hall#")
      println(s"###${room1.head}#${room2.head}#${room3.head}#${room4.head}###")
      println(s"  #${room1(1)}#${room2(1)}#${room3(1)}#${room4(1)}#   ")
      println("  #########  ")
    }
    def printAll(): Unit = {
      previous.foreach(_.printAll())
      println()
      print()
    }
    def moves: Vector[World] = {
      // Move into hall
      val movesIntoHall: Vector[World] = {
        (room1 match {
          case ".." | "AA" | ".A" => Vector.empty
          case s".$pod" => Vector(
            if (hall(0) == '.' && hall(1) == '.') Some(copy(hall.updated(0, pod.head), room1 = "..", energy = energy + 4 * cost(pod.head), previous = Some(this))) else None,
            if (hall(1) == '.') Some(copy(hall.updated(1, pod.head), room1 = "..", energy = energy + 3 * cost(pod.head), previous = Some(this))) else None,
            if (hall(3) == '.') Some(copy(hall.updated(3, pod.head), room1 = "..", energy = energy + 3 * cost(pod.head), previous = Some(this))) else None,
            if (hall(3) == '.' && hall(5) == '.') Some(copy(hall.updated(5, pod.head), room1 = "..", energy = energy + 5 * cost(pod.head), previous = Some(this))) else None,
            if (hall(3) == '.' && hall(5) == '.' && hall(7) == '.') Some(copy(hall.updated(7, pod.head), room1 = "..", energy = energy + 7 * cost(pod.head), previous = Some(this))) else None,
            if (hall(3) == '.' && hall(5) == '.' && hall(7) == '.' && hall(9) == '.') Some(copy(hall.updated(9, pod.head), room1 = "..", energy = energy + 9 * cost(pod.head), previous = Some(this))) else None,
            if (hall(3) == '.' && hall(5) == '.' && hall(7) == '.' && hall(9) == '.' && hall(10) == '.') Some(copy(hall.updated(10, pod.head), room1 = "..", energy = energy + 10 * cost(pod.head), previous = Some(this))) else None
          ).flatten
          case full => Vector(
            if (hall(0) == '.' && hall(1) == '.') Some(copy(hall.updated(0, full.head), room1 = room1.updated(0, '.'), energy = energy + 3 * cost(full.head), previous = Some(this))) else None,
            if (hall(1) == '.') Some(copy(hall.updated(1, full.head), room1 = room1.updated(0, '.'), energy = energy + 2 * cost(full.head), previous = Some(this))) else None,
            if (hall(3) == '.') Some(copy(hall.updated(3, full.head), room1 = room1.updated(0, '.'), energy = energy + 2 * cost(full.head), previous = Some(this))) else None,
            if (hall(3) == '.' && hall(5) == '.') Some(copy(hall.updated(5, full.head), room1 = room1.updated(0, '.'), energy = energy + 4 * cost(full.head), previous = Some(this))) else None,
            if (hall(3) == '.' && hall(5) == '.' && hall(7) == '.') Some(copy(hall.updated(7, full.head), room1 = room1.updated(0, '.'), energy = energy + 6 * cost(full.head), previous = Some(this))) else None,
            if (hall(3) == '.' && hall(5) == '.' && hall(7) == '.' && hall(9) == '.') Some(copy(hall.updated(9, full.head), room1 = room1.updated(0, '.'), energy = energy + 8 * cost(full.head), previous = Some(this))) else None,
            if (hall(3) == '.' && hall(5) == '.' && hall(7) == '.' && hall(9) == '.' && hall(10) == '.') Some(copy(hall.updated(10, full.head), room1 = room1.updated(0, '.'), energy = energy + 9 * cost(full.head), previous = Some(this))) else None
          ).flatten
        }) ++ (room2 match {
          case ".." | "BB" | ".B" => Vector.empty
          case s".$pod" => Vector(
            if (hall(0) == '.' && hall(1) == '.' && hall(3) == '.') Some(copy(hall.updated(0, pod.head), room2 = "..", energy = energy + 6 * cost(pod.head), previous = Some(this))) else None,
            if (hall(1) == '.' && hall(3) == '.') Some(copy(hall.updated(1, pod.head), room2 = "..", energy = energy + 5 * cost(pod.head), previous = Some(this))) else None,
            if (hall(3) == '.') Some(copy(hall.updated(3, pod.head), room2 = "..", energy = energy + 3 * cost(pod.head), previous = Some(this))) else None,
            if (hall(5) == '.') Some(copy(hall.updated(5, pod.head), room2 = "..", energy = energy + 3 * cost(pod.head), previous = Some(this))) else None,
            if (hall(5) == '.' && hall(7) == '.') Some(copy(hall.updated(7, pod.head), room2 = "..", energy = energy + 5 * cost(pod.head), previous = Some(this))) else None,
            if (hall(5) == '.' && hall(7) == '.' && hall(9) == '.') Some(copy(hall.updated(9, pod.head), room2 = "..", energy = energy + 7 * cost(pod.head), previous = Some(this))) else None,
            if (hall(5) == '.' && hall(7) == '.' && hall(9) == '.' && hall(10) == '.') Some(copy(hall.updated(10, pod.head), room2 = "..", energy = energy + 8 * cost(pod.head), previous = Some(this))) else None
          ).flatten
          case full => Vector(
            if (hall(0) == '.' && hall(1) == '.' && hall(3) == '.') Some(copy(hall.updated(0, full.head), room2 = room2.updated(0, '.'), energy = energy + 5 * cost(full.head), previous = Some(this))) else None,
            if (hall(1) == '.' && hall(3) == '.') Some(copy(hall.updated(1, full.head), room2 = room2.updated(0, '.'), energy = energy + 4 * cost(full.head), previous = Some(this))) else None,
            if (hall(3) == '.') Some(copy(hall.updated(3, full.head), room2 = room2.updated(0, '.'), energy = energy + 2 * cost(full.head), previous = Some(this))) else None,
            if (hall(5) == '.') Some(copy(hall.updated(5, full.head), room2 = room2.updated(0, '.'), energy = energy + 2 * cost(full.head), previous = Some(this))) else None,
            if (hall(5) == '.' && hall(7) == '.') Some(copy(hall.updated(7, full.head), room2 = room2.updated(0, '.'), energy = energy + 4 * cost(full.head), previous = Some(this))) else None,
            if (hall(5) == '.' && hall(7) == '.' && hall(9) == '.') Some(copy(hall.updated(9, full.head), room2 = room2.updated(0, '.'), energy = energy + 6 * cost(full.head), previous = Some(this))) else None,
            if (hall(5) == '.' && hall(7) == '.' && hall(9) == '.' && hall(10) == '.') Some(copy(hall.updated(10, full.head), room2 = room2.updated(0, '.'), energy = energy + 7 * cost(full.head), previous = Some(this))) else None
          ).flatten
        }) ++ (room3 match {
          case ".." | "CC" | ".C" => Vector.empty
          case s".$pod" => Vector(
            if (hall(0) == '.' && hall(1) == '.' && hall(3) == '.' && hall(5) == '.') Some(copy(hall.updated(0, pod.head), room3 = "..", energy = energy + 8 * cost(pod.head), previous = Some(this))) else None,
            if (hall(1) == '.' && hall(3) == '.' && hall(5) == '.') Some(copy(hall.updated(1, pod.head), room3 = "..", energy = energy + 7 * cost(pod.head), previous = Some(this))) else None,
            if (hall(3) == '.' && hall(5) == '.') Some(copy(hall.updated(3, pod.head), room3 = "..", energy = energy + 5 * cost(pod.head), previous = Some(this))) else None,
            if (hall(5) == '.') Some(copy(hall.updated(5, pod.head), room3 = "..", energy = energy + 3 * cost(pod.head), previous = Some(this))) else None,
            if (hall(7) == '.') Some(copy(hall.updated(7, pod.head), room3 = "..", energy = energy + 3 * cost(pod.head), previous = Some(this))) else None,
            if (hall(7) == '.' && hall(9) == '.') Some(copy(hall.updated(9, pod.head), room3 = "..", energy = energy + 5 * cost(pod.head), previous = Some(this))) else None,
            if (hall(7) == '.' && hall(9) == '.' && hall(10) == '.') Some(copy(hall.updated(10, pod.head), room3 = "..", energy = energy + 6 * cost(pod.head), previous = Some(this))) else None
          ).flatten
          case full => Vector(
            if (hall(0) == '.' && hall(1) == '.' && hall(3) == '.' && hall(5) == '.') Some(copy(hall.updated(0, full.head), room3 = room3.updated(0, '.'), energy = energy + 7 * cost(full.head), previous = Some(this))) else None,
            if (hall(1) == '.' && hall(3) == '.' && hall(5) =='.') Some(copy(hall.updated(1, full.head), room3 = room3.updated(0, '.'), energy = energy + 6 * cost(full.head), previous = Some(this))) else None,
            if (hall(3) == '.' && hall(5) == '.') Some(copy(hall.updated(3, full.head), room3 = room3.updated(0, '.'), energy = energy + 4 * cost(full.head), previous = Some(this))) else None,
            if (hall(5) == '.') Some(copy(hall.updated(5, full.head), room3 = room3.updated(0, '.'), energy = energy + 2 * cost(full.head), previous = Some(this))) else None,
            if (hall(7) == '.') Some(copy(hall.updated(7, full.head), room3 = room3.updated(0, '.'), energy = energy + 2 * cost(full.head), previous = Some(this))) else None,
            if (hall(7) == '.' && hall(9) == '.') Some(copy(hall.updated(9, full.head), room3 = room3.updated(0, '.'), energy = energy + 4 * cost(full.head), previous = Some(this))) else None,
            if (hall(7) == '.' && hall(9) == '.' && hall(10) == '.') Some(copy(hall.updated(10, full.head), room3 = room3.updated(0, '.'), energy = energy + 5 * cost(full.head), previous = Some(this))) else None
          ).flatten
        }) ++ (room4 match {
          case ".." | "DD" | ".D" => Vector.empty
          case s".$pod" => Vector(
            if (hall(0) == '.' && hall(1) == '.' && hall(3) == '.' && hall(5) == '.' && hall(7) == '.') Some(copy(hall.updated(0, pod.head), room4 = "..", energy = energy + 10 * cost(pod.head), previous = Some(this))) else None,
            if (hall(1) == '.' && hall(3) == '.' && hall(5) == '.' && hall(7) == '.') Some(copy(hall.updated(1, pod.head), room4 = "..", energy = energy + 9 * cost(pod.head), previous = Some(this))) else None,
            if (hall(3) == '.' && hall(5) == '.' && hall(7) == '.') Some(copy(hall.updated(3, pod.head), room4 = "..", energy = energy + 7 * cost(pod.head), previous = Some(this))) else None,
            if (hall(5) == '.' && hall(7) == '.') Some(copy(hall.updated(5, pod.head), room4 = "..", energy = energy + 5 * cost(pod.head), previous = Some(this))) else None,
            if (hall(7) == '.') Some(copy(hall.updated(7, pod.head), room4 = "..", energy = energy + 3 * cost(pod.head), previous = Some(this))) else None,
            if (hall(9) == '.') Some(copy(hall.updated(9, pod.head), room4 = "..", energy = energy + 3 * cost(pod.head), previous = Some(this))) else None,
            if (hall(9) == '.' && hall(10) == '.') Some(copy(hall.updated(10, pod.head), room4 = "..", energy = energy + 4 * cost(pod.head), previous = Some(this))) else None
          ).flatten
          case full => Vector(
            if (hall(0) == '.' && hall(1) == '.' && hall(3) == '.' && hall(5) == '.' && hall(7) == '.') Some(copy(hall.updated(0, full.head), room4 = room4.updated(0, '.'), energy = energy + 9 * cost(full.head), previous = Some(this))) else None,
            if (hall(1) == '.' && hall(3) == '.' && hall(5) == '.' && hall(7) == '.') Some(copy(hall.updated(1, full.head), room4 = room4.updated(0, '.'), energy = energy + 8 * cost(full.head), previous = Some(this))) else None,
            if (hall(3) == '.' && hall(5) == '.' && hall(7) == '.') Some(copy(hall.updated(3, full.head), room4 = room4.updated(0, '.'), energy = energy + 6 * cost(full.head), previous = Some(this))) else None,
            if (hall(5) == '.' && hall(7) == '.') Some(copy(hall.updated(5, full.head), room4 = room4.updated(0, '.'), energy = energy + 4 * cost(full.head), previous = Some(this))) else None,
            if (hall(7) == '.') Some(copy(hall.updated(7, full.head), room4 = room4.updated(0, '.'), energy = energy + 2 * cost(full.head), previous = Some(this))) else None,
            if (hall(9) == '.') Some(copy(hall.updated(9, full.head), room4 = room4.updated(0, '.'), energy = energy + 2 * cost(full.head), previous = Some(this))) else None,
            if (hall(9) == '.' && hall(10) == '.') Some(copy(hall.updated(10, full.head), room4 = room4.updated(0, '.'), energy = energy + 3 * cost(full.head), previous = Some(this))) else None
          ).flatten
        })
      }
      // Move into room
      val movesIntoRoom: Vector[World] = hall.zipWithIndex.toVector.flatMap {
        case ('A', i) if room1 == ".." && (2 until i).map(hall(_)).forall(_ == '.') && (i+1 to 2).map(hall(_)).forall(_ == '.') => Some(copy(hall = hall.updated(i, '.'), room1 = ".A", energy = energy + (2 + (i - 2).abs), previous = Some(this)))
        case ('A', i) if room1 == ".A" && (2 until i).map(hall(_)).forall(_ == '.') && (i+1 to 2).map(hall(_)).forall(_ == '.') => Some(copy(hall = hall.updated(i, '.'), room1 = "AA", energy = energy + (1 + (i - 2).abs), previous = Some(this)))
        case ('B', i) if room2 == ".." && (4 until i).map(hall(_)).forall(_ == '.') && (i+1 to 4).map(hall(_)).forall(_ == '.') => Some(copy(hall = hall.updated(i, '.'), room2 = ".B", energy = energy + ((2 + (i - 4).abs) * 10), previous = Some(this)))
        case ('B', i) if room2 == ".B" && (4 until i).map(hall(_)).forall(_ == '.') && (i+1 to 4).map(hall(_)).forall(_ == '.') => Some(copy(hall = hall.updated(i, '.'), room2 = "BB", energy = energy + ((1 + (i - 4).abs) * 10), previous = Some(this)))
        case ('C', i) if room3 == ".." && (6 until i).map(hall(_)).forall(_ == '.') && (i+1 to 6).map(hall(_)).forall(_ == '.') => Some(copy(hall = hall.updated(i, '.'), room3 = ".C", energy = energy + ((2 + (i - 6).abs) * 100), previous = Some(this)))
        case ('C', i) if room3 == ".C" && (6 until i).map(hall(_)).forall(_ == '.') && (i+1 to 6).map(hall(_)).forall(_ == '.') => Some(copy(hall = hall.updated(i, '.'), room3 = "CC", energy = energy + ((1 + (i - 6).abs) * 100), previous = Some(this)))
        case ('D', i) if room4 == ".." && (8 until i).map(hall(_)).forall(_ == '.') && (i+1 to 8).map(hall(_)).forall(_ == '.') => Some(copy(hall = hall.updated(i, '.'), room4 = ".D", energy = energy + ((2 + (i - 8).abs) * 1000), previous = Some(this)))
        case ('D', i) if room4 == ".D" && (8 until i).map(hall(_)).forall(_ == '.') && (i+1 to 8).map(hall(_)).forall(_ == '.') => Some(copy(hall = hall.updated(i, '.'), room4 = "DD", energy = energy + ((1 + (i - 8).abs) * 1000), previous = Some(this)))
        case _ => None
      }
      movesIntoHall ++ movesIntoRoom
    }
  }

//  val input = World("...........", "BA", "CD", "BC", "DA", 0L, None)
  val input = World("...........", "CB", "DA", "DB", "AC", 0L, None)

  input.print()
  println()

  //val moves = input.moves
  //moves.foreach(_.print())

  //println("------")
  //moves(7).moves.foreach(_.print())

  val allSolutions = mutable.Set.empty[World]
  val seen = mutable.Map.empty[(String, String, String, String, String), Long]
  val todo = mutable.Set[World](input)
  var bestSoFar = Long.MaxValue

  while (todo.nonEmpty) {
    val w = todo.head
    todo.remove(w)
    if (w.energy < bestSoFar) {
      w.moves.map { world =>
        if (world.complete) {
//          if (world.energy < 12521) {
//            println(s"Complete: ${world.energy}")
//            world.printAll()
//          }
          bestSoFar = bestSoFar.min(world.energy)
          println(s"Best so far: $bestSoFar")
          allSolutions.add(world)
        } else {
          if (!seen.get(world.hall, world.room1, world.room2, world.room3, world.room4).exists(_ <= world.energy)) {
            todo.add(world)
            seen.addOne((world.hall, world.room1, world.room2, world.room3, world.room4), world.energy)
          }
        }
      }
    }
  }

  val answer = allSolutions.minBy(_.energy)
  println(s"Answer1: ${answer.energy}")
  answer.printAll()

}
