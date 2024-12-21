import scala.collection.mutable
import scala.io.Source

object Problem21 extends App:
//  val file = "21-test.txt"
  val file = "21-input.txt"
  val input = Source.fromResource(file).getLines().toVector

  def numSeqs(s: String): Vector[Vector[Char]] =
    ('A' + s).sliding(2).foldLeft(Vector(Vector.empty[Char])) { case (acc, pair) =>
      val paths = numPaths(pair(0), pair(1))
      paths.flatMap { path =>
        acc.map(_ ++ path)
      }
    }

  def numY(c: Char): Int = c match
    case '7' | '8' | '9' => 0
    case '4' | '5' | '6' => 1
    case '1' | '2' | '3' => 2
    case '0' | 'A' => 3

  def numX(c: Char): Int = c match
    case '1' | '4' | '7' => 0
    case '0' | '2' | '5' | '8' => 1
    case 'A' | '3' | '6' | '9' => 2

  def numHasGap(y: Int, x: Int, buttons: Vector[Char]): Boolean = {
    val result = buttons.foldLeft((y, x, false))( (acc, c) =>
      val (y, x, gap) = acc
      val ny = c match {
        case 'v' => y + 1
        case '^' => y - 1
        case _ => y
      }
      val nx = c match {
        case '>' => x + 1
        case '<' => x - 1
        case _ => x
      }
      (ny, nx, gap || (ny == 3 && nx == 0))
    )
    result._3
  }

  val numPathsCache = mutable.Map.empty[(Char, Char), Vector[Vector[Char]]]
  def numPaths(a: Char, b: Char): Vector[Vector[Char]] =
    numPathsCache.getOrElseUpdate((a, b), {
      val ay = numY(a)
      val ax = numX(a)
      val by = numY(b)
      val bx = numX(b)

      val vertical = Vector.fill((ay - by).abs)(if ay < by then 'v' else '^')
      val horizontal = Vector.fill((ax - bx).abs)(if ax < bx then '>' else '<')
      val path = vertical ++ horizontal
      val allPaths = path.permutations.toVector
      allPaths.filterNot(numHasGap(ay, ax, _)).map(_.appended('A'))
    })

  def dirSeqs(s: Vector[Char]): Vector[Vector[Char]] =
    ('A' +: s).sliding(2).foldLeft(Vector(Vector.empty[Char])) { case (acc, pair) =>
      val paths = dirPaths(pair(0), pair(1))
      paths.flatMap { path =>
        acc.map(_ ++ path)
      }
    }

  def dirY(c: Char): Int = c match
    case '^' | 'A' => 0
    case '<' | 'v' | '>' => 1

  def dirX(c: Char): Int = c match
    case '<' => 0
    case '^' | 'v' => 1
    case 'A' | '>' => 2

  def dirHasGap(y: Int, x: Int, buttons: Vector[Char]): Boolean = {
    val result = buttons.foldLeft((y, x, false))((acc, c) =>
      val (y, x, gap) = acc
      val ny = c match {
        case 'v' => y + 1
        case '^' => y - 1
        case _ => y
      }
      val nx = c match {
        case '>' => x + 1
        case '<' => x - 1
        case _ => x
      }
      (ny, nx, gap || (ny == 0 && nx == 0))
    )
    result._3
  }

  val dirPathsCache = mutable.Map.empty[(Char, Char), Vector[Vector[Char]]]
  def dirPaths(a: Char, b: Char): Vector[Vector[Char]] =
    dirPathsCache.getOrElseUpdate((a, b), {
      val ay = dirY(a)
      val ax = dirX(a)
      val by = dirY(b)
      val bx = dirX(b)

      val vertical = Vector.fill((ay - by).abs)(if ay < by then 'v' else '^')
      val horizontal = Vector.fill((ax - bx).abs)(if ax < bx then '>' else '<')
      val path = vertical ++ horizontal
      val allPaths = path.permutations.toVector
      allPaths.filterNot(dirHasGap(ay, ax, _)).map(_.appended('A'))
    })
  // Naive solution
//  val result: Vector[Int] = input.map(code => numSeqs(code).flatMap(dirSeqs).flatMap(dirSeqs).map(_.length).min * code.take(3).toInt)
//  println(result.sum)

  // Smart solution :P
  def withIntermediateRobots(code: String, robotKeypads: Int): Long = {
    val cache = mutable.Map.empty[(Char, Char, Int), Long]
    for {
      c <- 0 until robotKeypads
      a <- Vector('A', 'v', '^', '<', '>')
      b <- Vector('A', 'v', '^', '<', '>')
    } {
      if c == 0 then
        cache((a, b, c)) = dirPaths(a, b).map(_.length).min
      else
        cache((a, b, c)) = dirPaths(a, b).map(path =>
          ('A' +: path).sliding(2).map(pair =>
            cache((pair(0), pair(1), c - 1))
          ).sum
        ).min
    }
    val res = numSeqs(code)
    res.map(path => ('A' +: path).sliding(2).map(pair =>
      cache((pair(0), pair(1), robotKeypads-1))
    ).sum).min * code.take(3).toLong
  }

  println(input.map(withIntermediateRobots(_, 2)).sum)
  println(input.map(withIntermediateRobots(_, 25)).sum)
