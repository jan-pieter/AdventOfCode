import scala.io.Source

object Problem15 extends App:
  val file = "15-test.txt"
//  val file = "15-input.txt"
  val input = Source.fromResource(file).getLines().toVector
  val grid = Grid(input.takeWhile(_.nonEmpty).map(_.toArray).toArray)
  val grid2 = Grid(grid.grid.map(_.clone()))
  val moves = input.dropWhile(_.nonEmpty).tail.reduce(_ ++ _).toVector

  def move2dir(move: Char) = move match {
    case '^' => -1 -> 0
    case 'v' => 1 -> 0
    case '<' => 0 -> -1
    case '>' => 0 -> 1
  }

  case class Grid(grid: Array[Array[Char]]) {
    private var robotPos = grid.indexWhere(_.contains('@')) -> grid.find(_.contains('@')).get.indexOf('@')
    def printGrid(): Unit = {
      grid.foreach { row =>
        println(row.mkString)
      }
    }
    def coordinates(box: Char): Long = {
      (for {
        y <- grid.indices
        x <- grid(y).indices
        if grid(y)(x) == box
      } yield y * 100L + x.toLong).sum
    }
    def move(step: Char): Unit = {
      val dir = move2dir(step)
      def moveWhenPossible(pos: (Int, Int)): Boolean = {
        val newPos = (pos._1 + dir._1) -> (pos._2 + dir._2)
        if grid(newPos._1)(newPos._2) == '#' then false
        else if grid(newPos._1)(newPos._2) == '.' then
          grid(newPos._1)(newPos._2) = grid(pos._1)(pos._2)
          grid(pos._1)(pos._2) = '.'
          true
        else
          if moveWhenPossible(newPos) then
            grid(newPos._1)(newPos._2) = grid(pos._1)(pos._2)
            grid(pos._1)(pos._2) = '.'
            true
          else false
      }

      if moveWhenPossible(robotPos) then
        robotPos = (robotPos._1 + dir._1) -> (robotPos._2 + dir._2)
    }
    def move2(step: Char): Grid = {
      val dir = move2dir(step)
      val newGrid = grid.map(_.clone)
      def moveWhenPossible(pos: (Int, Int)): Boolean = {
        val newPos = (pos._1 + dir._1) -> (pos._2 + dir._2)
        if newGrid(newPos._1)(newPos._2) == '#' then false
        else if newGrid(newPos._1)(newPos._2) == '.' then
          newGrid(newPos._1)(newPos._2) = newGrid(pos._1)(pos._2)
          newGrid(pos._1)(pos._2) = '.'
          true
        else if newGrid(newPos._1)(newPos._2) == ']' && (step == '<' || step == '>') then
          if moveWhenPossible(newPos) then
            newGrid(newPos._1)(newPos._2) = newGrid(pos._1)(pos._2)
            newGrid(pos._1)(pos._2) = '.'
            true
          else false
        else if newGrid(newPos._1)(newPos._2) == ']' then
          if moveWhenPossible(newPos) && moveWhenPossible(newPos._1 -> (newPos._2-1)) then
            newGrid(newPos._1)(newPos._2) = newGrid(pos._1)(pos._2)
            newGrid(pos._1)(pos._2) = '.'
            true
          else false
        else if newGrid(newPos._1)(newPos._2) == '[' && (step == '<' || step == '>') then
          if moveWhenPossible(newPos) then
            newGrid(newPos._1)(newPos._2) = newGrid(pos._1)(pos._2)
            newGrid(pos._1)(pos._2) = '.'
            true
          else false
        else
          if moveWhenPossible(newPos) && moveWhenPossible(newPos._1 -> (newPos._2 + 1)) then
            newGrid(newPos._1)(newPos._2) = newGrid(pos._1)(pos._2)
            newGrid(pos._1)(pos._2) = '.'
            true
          else false
      }

      if moveWhenPossible(robotPos) then
        Grid(newGrid)
      else this
    }
  }
  val result = moves.foldLeft(grid){(g, m) => g.move(m); g}.coordinates('O')
  println(result)

  val newGridArray = Array.fill(grid2.grid.length, grid2.grid.head.length*2)('.')
  for {
    y <- grid2.grid.indices
    x <- grid2.grid(y).indices
  } yield grid2.grid(y)(x) match {
    case '#' => newGridArray(y)(x*2) = '#'; newGridArray(y)(x*2+1) = '#'
    case '.' => newGridArray(y)(x*2) = '.'; newGridArray(y)(x*2+1) = '.'
    case '@' => newGridArray(y)(x*2) = '@'; newGridArray(y)(x*2+1) = '.'
    case 'O' => newGridArray(y)(x*2) = '['; newGridArray(y)(x*2+1) = ']'
  }
  val newGrid = Grid(newGridArray)
  val result2 = moves.foldLeft(newGrid){(g, m) => g.move2(m)}.coordinates('[')
  println(result2)
