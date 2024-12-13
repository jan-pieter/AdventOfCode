import scala.io.Source

object Problem13 extends App:
//  val file = "13-test.txt"
  val file = "13-input.txt"
  val input = Source.fromResource(file).getLines().toVector

  def gcd(x: Long, y: Long): Long = {
    val bigX = BigInt(x)
    val bigY = BigInt(y)
    bigX.gcd(bigY).longValue
  }
  def lcm(a: Long, b: Long): Long = (a*b).abs / gcd(a, b)

  def naive(ax: Int, ay: Int, bx: Int, by: Int, prizeX: Int, prizeY: Int) =
    val answers = for {
      a <- 0 to Math.min(prizeX / ax, prizeY / ay)
      b <- 0 to Math.min(prizeX / bx, prizeY / by)
      if ax * a + bx * b == prizeX && ay * a + by * b == prizeY
    } yield (a, b)
//    println(answers)
    answers.map((a, b) => a * 3 + b).minOption

  case class Game(ax: Long, ay: Long, bx: Long, by: Long, prizeX: Long, prizeY: Long) {
    def minimalCost: Option[Int] = naive(ax.toInt, ay.toInt, bx.toInt, by.toInt, prizeX.toInt, prizeY.toInt)
    def minimalCost2: Option[Long] = {
      val scaledAx = ax * -by
      val scaledPrizeX = prizeX * -by
      val scaledBx = bx * ay
      val scaledPrizeY = prizeY * bx

      val aFactor = scaledBx + scaledAx
      val prizeFactor = scaledPrizeY+scaledPrizeX
      val a = prizeFactor / aFactor
      val remainder = prizeX - a*ax
//      println(s"$ax $ay $bx $by $prizeX $prizeY $scaledAx $scaledPrizeX $scaledBx $scaledPrizeY $aFactor $prizeFactor $a $remainder")

      val scaledAy = ay * -bx
      val scaledPrizeY2 = prizeY * -bx
      val scaledBy = by * ax
      val scaledPrizeX2 = prizeX * by
      val bFactor = scaledBy+scaledAy
      val prizeFactor2 = scaledPrizeY2+scaledPrizeX2
      val b = prizeFactor2 / bFactor
      val remainder2 = prizeY - b*ay


      if remainder % bx == 0 && remainder2 % by == 0 then
        Some(a*3 + remainder / bx)
      else
        None
    }

    // a * ax + b * bx = prizeX
    // a * ay + b * by = prizeY

    // a * 94 + b * 22 = 8400
    // a * 34 + b * 67 = 5400

    // a * (94*-67=−6298) + b * (22*-67=−1474) = (8400 * -67=−562800)
    // a * (34*22=748) + b * (67*22=1474) = (5400*22=118800)

    // a * (748-6298=−5530) = (118800-562800=-444000)

    // 94 * 3 * 22 = 6204
    // 34 * 3 * 67 = 6846

    // a * 62 + b * 14 = 19256
    // a * 14 + b * 71 = 17168

    // a * (62*-71=-4402) + b * (14*-71=-994) = (19256 * -71=-1367176)
    // a * (14*14=196) + b * (71*14=994) = (17168*14=240352)

    // a * (196-4402=-4206) = (240352-1367176=-1126824)
  }

  val games = input.filter(_.nonEmpty).grouped(3).map { g =>
    val (ax, ay) = g(0) match {
      case s"Button A: X+$ax, Y+$ay" => (ax.toInt, ay.toInt)
    }
    val (bx, by) = g(1) match {
      case s"Button B: X+$bx, Y+$by" => (bx.toInt, by.toInt)
    }
    val (prizeX, prizeY) = g(2) match {
      case s"Prize: X=$prizeX, Y=$prizeY" => (prizeX.toInt, prizeY.toInt)
    }
    Game(ax, ay, bx, by, prizeX, prizeY)
  }.toVector

//  println(games.map(_.minimalCost))
//  println(games.map(_.minimalCost2))
  println(games.flatMap(_.minimalCost).sum)
//  println(games.map(game => game.copy(prizeX = game.prizeX+10000000000000L, prizeY = game.prizeY+10000000000000L)).map(_.minimalCost2))
  println(games.map(game => game.copy(prizeX = game.prizeX+10000000000000L, prizeY = game.prizeY+10000000000000L)).flatMap(_.minimalCost2).sum)