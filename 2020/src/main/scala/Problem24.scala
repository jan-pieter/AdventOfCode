import scala.io.Source

object Problem24 extends App {

  val input = Source.fromResource("24-input.txt").getLines().toList

  val size = 2048
  val board: Array[Array[Boolean]] = Array.fill(size, size)(true)
  val startQ, startR = size / 2

  input.foreach { inLine =>
    var current: (Int, Int) = (startQ, startR)
    var lineIndex = 0
    val line = inLine + " "
    while (lineIndex < inLine.length) {
      line.slice(lineIndex, lineIndex + 2) match {
        case "se" =>
          current = (current._1, current._2 + 1)
          lineIndex = lineIndex + 2
        case "sw" =>
          current = (current._1 - 1, current._2 + 1)
          lineIndex = lineIndex + 2
        case "ne" =>
          current = (current._1 + 1, current._2 - 1)
          lineIndex = lineIndex + 2
        case "nw" =>
          current = (current._1, current._2 - 1)
          lineIndex = lineIndex + 2
        case other if other.head == 'e' =>
          current = (current._1 + 1, current._2)
          lineIndex = lineIndex + 1
        case other if other.head == 'w' =>
          current = (current._1 - 1, current._2)
          lineIndex = lineIndex + 1
      }
    }
    board(current._1)(current._2) = !board(current._1)(current._2)
  }

  def blackTiles(board: Array[Array[Boolean]]): Int = board.map(_.map {
    case false => 1
    case true => 0
  }.sum).sum

  def safeBlack(board: Array[Array[Boolean]], q: Int, r: Int): Int = {
    if (q > 0 && q < board.length && r > 0 && r < board.length)
      if (board(q)(r)) 0 else 1
    else
      0
  }

  def blackNeighbors(board: Array[Array[Boolean]], q: Int, r: Int): Int = {
    safeBlack(board, q, r-1) + safeBlack(board, q+1, r-1) + safeBlack(board, q+1, r) +
      safeBlack(board, q, r+1) + safeBlack(board, q-1, r+1) + safeBlack(board, q-1, r)
  }

  println(blackTiles(board))

  val endBoard = (0 until 100).foldLeft(board) {
    case (prevBoard, _) =>
      val newBoard = Array.fill[Boolean](size, size)(true)
      for {
        q <- newBoard.indices
        r <- newBoard.indices
      } yield {
        val black = blackNeighbors(prevBoard, q, r)
        if (prevBoard(q)(r) && black == 2)
          newBoard(q)(r) = false
        else if (!prevBoard(q)(r) && (black == 0 || black > 2))
          newBoard(q)(r) = true
        else
          newBoard(q)(r) = prevBoard(q)(r)
      }
      //println(blackTiles(newBoard))
      newBoard
  }

  println(blackTiles(endBoard))


}
