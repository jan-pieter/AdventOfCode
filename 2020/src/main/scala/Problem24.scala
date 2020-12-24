import scala.io.Source

object Problem24 extends App {

  val input = Source.fromResource("24-test.txt").getLines().toList

  val size = 128
  val board: Array[Array[Array[Boolean]]] = Array.fill(size, size, size)(true)
  val startX, startY, startZ = size / 2

  input.foreach { inLine =>
    var current: (Int, Int, Int) = (startX, startY, startZ)
    var lineIndex = 0
    val line = inLine + " "
    while (lineIndex < inLine.length) {
      line.slice(lineIndex, lineIndex + 2) match {
        case "se" =>
          current = (current._1, current._2 - 1, current._3 + 1)
          lineIndex = lineIndex + 2
        case "sw" =>
          current = (current._1 - 1, current._2, current._3 + 1)
          lineIndex = lineIndex + 2
        case "ne" =>
          current = (current._1 + 1, current._2, current._3 - 1)
          lineIndex = lineIndex + 2
        case "nw" =>
          current = (current._1, current._2 + 1, current._3 - 1)
          lineIndex = lineIndex + 2
        case other if other.head == 'e' =>
          current = (current._1 + 1, current._2 - 1, current._3)
          lineIndex = lineIndex + 1
        case other if other.head == 'w' =>
          current = (current._1 - 1, current._2 + 1, current._3)
          lineIndex = lineIndex + 1
      }
    }
    board(current._3)(current._2)(current._1) = !board(current._3)(current._2)(current._1)
  }

  def blackTiles(board: Array[Array[Array[Boolean]]]): Int = board.map(_.map(_.map {
    case false => 1
    case true => 0
  }.sum).sum).sum

  def safeBlack(board: Array[Array[Array[Boolean]]], x: Int, y: Int, z: Int): Int = {
    if (z > 0 && z < board.length && y > 0 && y < board.length && x > 0 && x < board.length)
      if (board(z)(y)(x)) 0 else 1
    else
      0
  }

  def blackNeighbors(board: Array[Array[Array[Boolean]]], x: Int, y: Int, z: Int): Int = {
    safeBlack(board, x, y+1, z-1) + safeBlack(board, x+1, y, z-1) + safeBlack(board, x+1, y-1, z) +
      safeBlack(board, x, y-1, z+1) + safeBlack(board, x-1, y, z+1) + safeBlack(board, x-1, y+1, z)
  }

  println(blackTiles(board))

  val endBoard = (0 until 10).foldLeft(board) {
    case (prevBoard, _) =>
      val newBoard = Array.fill[Boolean](size, size, size)(true)
      for {
        y <- newBoard.indices
        x <- newBoard.indices
      } yield {
        val z = 0 - (x + y)
        val black = blackNeighbors(prevBoard, x, y, z)
        if (prevBoard(z)(y)(x) && black == 2)
          newBoard(z)(y)(x) = false
        else if (!prevBoard(z)(y)(x) && (black == 0 || black > 2))
          newBoard(z)(y)(x) = true
        else
          newBoard(z)(y)(x) = prevBoard(z)(y)(x)
      }
      println(blackTiles(newBoard))
      newBoard
  }

  println(blackTiles(endBoard))


}
