import scala.collection.mutable
import scala.io.Source

object Problem24 extends App {

  val start = Source.fromResource("24-input.txt").getLines().toVector
  val width = 5
  val height = 5
  
  case class Level(world: Vector[String], var child: Option[Level], var parent: Option[Level])
  
  def neighbours(world: Vector[String], x: Int, y: Int): String = {
    (for {
      (xDiff,yDiff) <- List((-1, 0), (1, 0), (0,-1), (0, 1))
      newX = x + xDiff
      newY = y + yDiff
      if newX >= 0 && newX < width
      if newY >=0 && newY < height
    } yield world(newY)(newX)).mkString
  }

  def neighbours(level: Level, x: Int, y: Int): String = {
    (for {
      (xDiff,yDiff) <- List((-1, 0), (1, 0), (0,-1), (0, 1))
      newX = x + xDiff
      newY = y + yDiff
    } yield {
      if (newX == -1) {
        level.parent.map(_.world(2)(1)).mkString
      } else if (newX == width) {
        level.parent.map(_.world(2)(3)).mkString
      } else if (newY == -1) {
        level.parent.map(_.world(1)(2)).mkString
      } else if (newY == height) {
        level.parent.map(_.world(3)(2)).mkString
      } else if (newX == 2 && newY == 2 && xDiff == 0 && yDiff == 1) {
        level.child.map(_.world(0)).getOrElse("")
      } else if (newX == 2 && newY == 2 && xDiff == 0 && yDiff == -1) {
        level.child.map(_.world(height-1)).getOrElse("")
      } else if (newX == 2 && newY == 2 && xDiff == 1 && yDiff == 0) {
        level.child.map(_.world.map(_.head).mkString).getOrElse("")
      } else if (newX == 2 && newY == 2 && xDiff == -1 && yDiff == 0) {
        level.child.map(_.world.map(_(width-1)).mkString).getOrElse("")
      } else {
        level.world(newY)(newX).toString
      }
    }).flatten.mkString
  }

  def cellStep(world: Vector[String], x: Int, y: Int): Char = {
    world(y)(x) match {
      case '#' if neighbours(world, x, y).count(_ == '#') == 1 => '#'
      case '#' => '.'
      case '.' if neighbours(world, x, y).count(_ == '#') == 1 || neighbours(world, x, y).count(_ == '#') == 2 => '#'
      case '.' => '.'
    }
  }
  
  def cellStep(level: Level, x: Int, y: Int): Char = {
    require(x != 2 || y != 2)
    level.world(y)(x) match {
      case '#' if neighbours(level, x, y).count(_ == '#') == 1 => '#'
      case '#' => '.'
      case '.' if neighbours(level, x, y).count(_ == '#') == 1 || neighbours(level, x, y).count(_ == '#') == 2 => '#'
      case '.' => '.'
    }
  }
  
  def step(world: Vector[String]): Vector[String] = {
    world.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (_, x) =>
        cellStep(world, x, y)
      }.mkString
    }
  }
  
  def step(level: Level, depth: Int = 0): Level = {
    val containsHashNearMiddle = level.world.slice(1, 4).map(_.substring(1, 4)).mkString.contains("#")
    val child = if (level.child.isDefined) {
      level.child
    } else if (containsHashNearMiddle) {
      Some(Level(Vector.fill(height)(List.fill(width)('.').mkString), None, Some(level)))
    } else {
      None
    }
    val containsHashOnOutside = (level.world(0) + level.world(height-1) + level.world.map(_.head).mkString + level.world.map(_(width-1)).mkString).contains('#')
    val parent = if (level.parent.isDefined) {
      level.parent
    } else if (containsHashOnOutside) {
      Some(Level(Vector.fill(height)(List.fill(width)('.').mkString), Some(level), None))
    } else {
      None
    }
    val newLevel = level.copy(child = child, parent = parent)
    val newWorld: Vector[String] = newLevel.world.zipWithIndex.map { case (row, y) =>
      row.zipWithIndex.map { case (_, x) =>
        if (y != 2 || x != 2) {
          cellStep(level, x, y)
        } else {
          '?'
        }
      }.mkString
    }
    val result = if (depth < 0) {
      newLevel.copy(world = newWorld, parent = newLevel.parent.map(step(_, depth - 1)))
    } else if (depth > 0) {
      newLevel.copy(world = newWorld, child = newLevel.child.map(step(_, depth + 1)))
    } else {
      newLevel.copy(world = newWorld, child = newLevel.child.map(step(_, depth + 1)), parent = newLevel.parent.map(step(_, depth - 1)))
    }
    result.parent.foreach(_.child = Some(result))
    result.child.foreach(_.parent = Some(result))
    result
  }
  
  val seen: mutable.Set[String] = mutable.Set.empty
  var state = start
  while(!seen.contains(state.mkString)) {
    seen.add(state.mkString)
    state = step(state)
  }
  
  def rating(world: Vector[String]): Long = {
    world.mkString.zipWithIndex.map {
      case ('#', index) => Math.pow(2, index).toLong
      case _ => 0L
    }.sum
  }
  
  println("World:")
  state.foreach(println)
  println(s"Rating: ${rating(state)}")

  val result = (0 until 200).foldLeft(Level(start, None, None)){(level, i) => 
    val r = step(level) 
//    println(i) 
//    printLevel(r) 
    r
  }
  
  def printLevel(level: Level, depth: Int = 0): Unit = {
    if (depth <= 0) {
      level.parent.foreach(printLevel(_, depth - 1))
      println()
    }
    println(s"Depth $depth:")
    level.world.foreach(println)
    if (depth >= 0) {
      println()
      level.child.foreach(printLevel(_, depth + 1))
    }
  }
  
//  printLevel(result)
  
  def countHashes(level: Level, depth: Int = 0): Long = {
    val parentCount = if (depth <= 0) level.parent.map(countHashes(_, depth - 1)).getOrElse(0L) else 0L
    val childCount = if (depth >= 0) level.child.map(countHashes(_, depth + 1)).getOrElse(0L) else 0L
    level.world.map(_.count(_ == '#')).sum + parentCount + childCount 
  }
  
  println(s"Hashes: ${countHashes(result)}")
  
  
}
