import scala.io.Source

object Problem22 extends App:
  val input = Source.fromResource("22-input.txt").getLines().toVector
  val (map, instructions) = input.splitAt(input.indexOf(""))

  val world = Array.fill(map.length + 2, map.head.length + 2)(' ')
  map.indices.foreach(y => map(y).indices.foreach(x => world(y+1)(x+1) = map(y)(x)))

  enum Direction:
    case N extends Direction
    case E extends Direction
    case S extends Direction
    case W extends Direction

  case class Position(x: Int, y: Int, direction: Direction, instructions: String) {
    def turnRight: Position = direction match {
      case Direction.N => copy(direction = Direction.E)
      case Direction.E => copy(direction = Direction.S)
      case Direction.S => copy(direction = Direction.W)
      case Direction.W => copy(direction = Direction.N)
    }

    def turnLeft: Position = direction match {
      case Direction.N => copy(direction = Direction.W)
      case Direction.E => copy(direction = Direction.N)
      case Direction.S => copy(direction = Direction.E)
      case Direction.W => copy(direction = Direction.S)
    }

    def move(steps: Int): Position = if (steps == 0) this else direction match {
      case Direction.N =>
        if (world(y - 1)(x) == '.') copy(y = y - 1).move(steps - 1)
        else if (world(y-1)(x) == '#') this
        else {
          val otherSide = world.transpose.apply(x).lastIndexWhere(c => c =='.' || c == '#')
          if (world(otherSide)(x) == '#') this
          else copy(y = otherSide).move(steps - 1)
        }
      case Direction.S =>
        if (world(y + 1)(x) == '.') copy(y = y + 1).move(steps - 1)
        else if (world(y + 1)(x) == '#') this
        else {
          val otherSide = world.transpose.apply(x).indexWhere(c => c == '.' || c == '#')
          if (world(otherSide)(x) == '#') this
          else copy(y = otherSide).move(steps - 1)
        }
      case Direction.W =>
        if (world(y)(x - 1) == '.') copy(x = x - 1).move(steps - 1)
        else if (world(y)(x - 1) == '#') this
        else {
          val otherSide = world(y).lastIndexWhere(c => c == '.' || c == '#')
          if (world(y)(otherSide) == '#') this
          else copy(x = otherSide).move(steps - 1)
        }
      case Direction.E =>
        if (world(y)(x+1) == '.') copy(x = x + 1).move(steps - 1)
        else if (world(y)(x+1) == '#') this
        else {
          val otherSide = world(y).indexWhere(c => c == '.' || c == '#')
          if (world(y)(otherSide) == '#') this
          else copy(x = otherSide).move(steps - 1)
        }
    }

    def move2(steps: Int): Position = if (steps == 0) this else direction match {
      case Direction.N =>
        if (world(y - 1)(x) == '.') copy(y = y - 1).move2(steps - 1)
        else if (world(y - 1)(x) == '#') this
        else {
          if (x <= 50) {
            val nextX = 51
            val nextY = x + 50
            if (world(nextY)(nextX) == '#') this
            else copy(y = nextY, x = nextX, direction = Direction.E).move2(steps - 1)
          } else if (x <= 100) {
            val nextX = 1
            val nextY = x + 100
            if (world(nextY)(nextX) == '#') this
            else copy(y = nextY, x = nextX, direction = Direction.E).move2(steps - 1)
          } else {
            val nextX = x - 100
            val nextY = 200
            if (world(nextY)(nextX) == '#') this
            else copy(y = nextY, x = nextX, direction = Direction.N).move2(steps - 1)
          }
        }
      case Direction.S =>
        if (world(y + 1)(x) == '.') copy(y = y + 1).move2(steps - 1)
        else if (world(y + 1)(x) == '#') this
        else {
          if (x <= 50) {
            val nextX = x + 100
            val nextY = 1
            if (world(nextY)(nextX) == '#') this
            else copy(y = nextY, x = nextX, direction = Direction.S).move2(steps - 1)
          } else if (x <= 100) {
            val nextX = 50
            val nextY = x + 100
            if (world(nextY)(nextX) == '#') this
            else copy(y = nextY, x = nextX, direction = Direction.W).move2(steps - 1)
          } else {
            val nextX = 100
            val nextY = x - 50
            if (world(nextY)(nextX) == '#') this
            else copy(y = nextY, x = nextX, direction = Direction.W).move2(steps - 1)
          }
        }
      case Direction.W =>
        if (world(y)(x - 1) == '.') copy(x = x - 1).move2(steps - 1)
        else if (world(y)(x - 1) == '#') this
        else {
          if (y <= 50) {
            val nextX = 1
            val nextY = (y - 151).abs
            if (world(nextY)(nextX) == '#') this
            else copy(y = nextY, x = nextX, direction = Direction.E).move2(steps - 1)
          } else if (y <= 100) {
            val nextX = y - 50
            val nextY = 101
            if (world(nextY)(nextX) == '#') this
            else copy(y = nextY, x = nextX, direction = Direction.S).move2(steps - 1)
          } else if (y <= 150) {
            val nextX = 51
            val nextY = (y - 151).abs
            if (world(nextY)(nextX) == '#') this
            else copy(y = nextY, x = nextX, direction = Direction.E).move2(steps - 1)
          } else {
            val nextX = y - 100
            val nextY = 1
            if (world(nextY)(nextX) == '#') this
            else copy(y = nextY, x = nextX, direction = Direction.S).move2(steps - 1)
          }
        }
      case Direction.E =>
        if (world(y)(x + 1) == '.') copy(x = x + 1).move2(steps - 1)
        else if (world(y)(x + 1) == '#') this
        else {
          if (y <= 50) {
            val nextX = 100
            val nextY = (y - 151).abs
            if (world(nextY)(nextX) == '#') this
            else copy(y = nextY, x = nextX, direction = Direction.W).move2(steps - 1)
          } else if (y <= 100) {
            val nextX = y + 50
            val nextY = 50
            if (world(nextY)(nextX) == '#') this
            else copy(y = nextY, x = nextX, direction = Direction.N).move2(steps - 1)
          } else if (y <= 150) {
            val nextX = 150
            val nextY = (y - 151).abs
            if (world(nextY)(nextX) == '#') this
            else copy(y = nextY, x = nextX, direction = Direction.W).move2(steps - 1)
          } else {
            val nextX = y - 100
            val nextY = 150
            if (world(nextY)(nextX) == '#') this
            else copy(y = nextY, x = nextX, direction = Direction.N).move2(steps - 1)
          }
        }
    }

    def hasNext: Boolean = instructions.nonEmpty
    def next: Position = instructions match {
      case s if s.startsWith("R") => turnRight.copy(instructions = s.drop(1))
      case s if s.startsWith("L") => turnLeft.copy(instructions = s.drop(1))
      case s => move(s.takeWhile(_.isDigit).toInt).copy(instructions = s.dropWhile(_.isDigit))
    }

    def next2: Position = instructions match {
      case s if s.startsWith("R") => turnRight.copy(instructions = s.drop(1))
      case s if s.startsWith("L") => turnLeft.copy(instructions = s.drop(1))
      case s => move2(s.takeWhile(_.isDigit).toInt).copy(instructions = s.dropWhile(_.isDigit))
    }

    def value: Long = y * 1000L + 4L * x + (direction match {
      case Direction.E => 0
      case Direction.S => 1
      case Direction.W => 2
      case Direction.N => 3
    })
  }

  var position = Position(world(1).indexOf('.'), 1, Direction.E, instructions(1))
  while (position.hasNext) {
    position = position.next
  }
  println(position.value)

  position = Position(world(1).indexOf('.'), 1, Direction.E, instructions(1))
  while (position.hasNext) {
    position = position.next2
  }
  println(position.value)
