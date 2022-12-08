import scala.io.Source

object Problem08 extends App:
  val input = Source.fromResource("08-input.txt").getLines().toVector.map(_.toVector.map(_.toInt))

  val visible: Array[Array[Boolean]] = Array.fill(input.length)(Array.fill(input.head.length)(false))

  def drawLine(row: Int, column: Int, directionX: Int, directionY: Int): Unit = {
    if (directionX == 1)
      input(row).zipWithIndex.foldLeft(-1) {
        case (highest, (height, index)) =>
          visible(row)(index) = visible(row)(index) || height > highest
          Math.max(height, highest)
      }
    else if (directionX == -1)
      input(row).zipWithIndex.foldRight(-1) {
        case ((height, index), highest) =>
          visible(row)(index) = visible(row)(index) || height > highest
          Math.max(height, highest)
      }
    else if (directionY == 1)
      input.transpose.apply(column).zipWithIndex.foldLeft(-1) {
        case (highest, (height, index)) =>
          visible(index)(column) = visible(index)(column) || height > highest
          Math.max(height, highest)
      }
    else if (directionY == -1)
      input.transpose.apply(column).zipWithIndex.foldRight(-1) {
        case ((height, index), highest) =>
          visible(index)(column) = visible(index)(column) || height > highest
          Math.max(height, highest)
      }
  }


  input.indices.foreach{row =>
    drawLine(row, 0, 1, 0)
    drawLine(row, input.head.length-1, -1, 0)
  }
  input.head.indices.foreach{ column =>
    drawLine(0, column, 0, 1)
    drawLine(input.length-1, column, 0, -1)
  }

  val solution1 = visible.map(_.count(identity)).sum
  println(solution1)

  def scenicScore(row: Int, column: Int): Long = {
    val up = (row to 0 by -1).foldLeft((0L, false)){
      case ((distance, done), newRow) if row == newRow => (distance, done)
      case ((distance, done), newRow) if !done => if (input(newRow)(column) < input(row)(column)) (distance + 1, done) else (distance + 1, true)
      case ((distance, done), newRow) => (distance, done)
    }._1
    val down = (row until input.length).foldLeft((0L, false)) {
      case ((distance, done), newRow) if row == newRow => (distance, done)
      case ((distance, done), newRow) if !done => if (input(newRow)(column) < input(row)(column)) (distance + 1, done) else (distance + 1, true)
      case ((distance, done), newRow) => (distance, done)
    }._1
    val left = (column to 0 by -1).foldLeft((0L, false)) {
      case ((distance, done), newColumn) if column == newColumn => (distance, done)
      case ((distance, done), newColumn) if !done => if (input(row)(newColumn) < input(row)(column)) (distance + 1, done) else (distance + 1, true)
      case ((distance, done), newColumn) => (distance, done)
    }._1
    val right = (column until input.head.length).foldLeft((0L, false)) {
      case ((distance, done), newColumn) if column == newColumn => (distance, done)
      case ((distance, done), newColumn) if !done => if (input(row)(newColumn) < input(row)(column)) (distance + 1, done) else (distance + 1, true)
      case ((distance, done), newColumn) => (distance, done)
    }._1

//    println(up)
//    println(down)
//    println(left)
//    println(right)

    up * down * left * right
  }

  val solution2 = (for {
    row <- input.indices
    column <- input.head.indices
  } yield scenicScore(row, column)).max
  println(solution2)
