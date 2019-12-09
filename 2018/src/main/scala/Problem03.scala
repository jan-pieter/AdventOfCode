import scala.io.Source

object Problem03 extends App {

  case class Claim(id: String, x: Int, y: Int, w: Int, h: Int)
  def claimFromString(str: String): Claim = {
    val parts = str.split(" ")
    val (x: Int, y: Int) = {
      val arr = parts(2).dropRight(1).split(",").take(2).map(_.toInt)
      arr(0) -> arr(1)
    }
    val (w: Int, h: Int) = {
      val arr = parts(3).split("x").take(2).map(_.toInt)
      arr(0) -> arr(1)
    }
    Claim(parts(0), x, y, w, h)
  }

  val claims = Source.fromResource("03-input.txt").getLines().map(claimFromString).toList

  val claimCounts = claims.foldLeft(Array.ofDim[Int](1000,1000)){(counts, claim) =>
    for {
      x <- claim.x until claim.x + claim.w
      y <- claim.y until claim.y + claim.h
    } yield {
      counts(x)(y) = counts(x)(y) + 1
    }
    counts
  }

  val doubleClaims = claimCounts.map(_.count(_ > 1)).sum

  println(s"Double claims: $doubleClaims")

  def checkClaim(claim: Claim): Unit = {
    if ((for {
      x <- claim.x until claim.x + claim.w
      y <- claim.y until claim.y + claim.h
    } yield {
      claimCounts(x)(y) == 1
    }).reduce(_ && _))
      println(s"Unique claim: $claim")
  }

  claims.foreach(checkClaim)

}
