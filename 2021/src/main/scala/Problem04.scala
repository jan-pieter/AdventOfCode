import scala.io.Source

case object Problem04 extends App {

  val input = Source.fromResource("04-input.txt").getLines()

  val drawn: Vector[Int] = input.take(1).toVector.head.split(",").map(_.toInt).toVector

  val boards: Vector[Array[Array[Int]]] = input.drop(1).filterNot(_.trim.isEmpty).grouped(5).map(lines => lines.map(_.trim.replace("  ", " ").split(" ").map(_.toInt)).toArray).toVector

  def won(board: Array[Array[Int]], numbers: Set[Int]): Boolean = {
    board.exists(_.forall(numbers.contains)) ||
      board.transpose.exists(_.forall(numbers.contains))
  }

  def score(board: Array[Array[Int]], numbers: Set[Int]): Int = {
    board.flatten.filterNot(numbers.contains).sum
  }

  case class State(winningBoard: Option[Array[Array[Int]]], numbers: List[Int])

  val endState = drawn.foldLeft(State(None, List.empty)){
    case (state, number) if state.winningBoard.isEmpty =>
      val newNumbers = number :: state.numbers
      boards.find(won(_, newNumbers.toSet)) match {
        case None => State(None, newNumbers)
        case Some(board) => State(Some(board), newNumbers)
      }
    case (state, _) => state
  }

  endState.winningBoard match {
    case Some(board) =>
      val endScore = score(board, endState.numbers.toSet) * endState.numbers.head
      println(s"Score $endScore")
    case None => println("No winner")
  }

  case class State2(boardsLeft: Vector[Array[Array[Int]]], winningBoard: Option[Array[Array[Int]]], numbers: List[Int])

  val endState2 = drawn.foldLeft(State2(boards, None, List.empty)){
    case (state, number) if state.winningBoard.isEmpty =>
      val newNumbers = number :: state.numbers
      val newBoardsLeft = state.boardsLeft.filterNot(won(_, newNumbers.toSet))
      if (newBoardsLeft.isEmpty) {
        println(s"Number $number, WON")
        println(s"Numbers $newNumbers")
        printBoard(state.boardsLeft.head)
        State2(newBoardsLeft, Some(state.boardsLeft.head), newNumbers)
      } else {
        println(s"Number $number NOT WON")
        State2(newBoardsLeft, None, newNumbers)
      }
    case (state, _) => state
  }

  endState2.winningBoard match {
    case Some(board) =>
      val endScore = score(board, endState2.numbers.toSet) * endState2.numbers.head
      println(s"Score $endScore")
    case None => println("No winner")
  }

  def printBoard(board: Array[Array[Int]]): Unit = {
    board.foreach(line => println(line.mkString(" ")))
  }
}
