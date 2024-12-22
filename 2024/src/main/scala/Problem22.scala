import scala.io.Source

object Problem22 extends App:
//  val file = "22-test.txt"
  val file = "22-input.txt"
  val input = Source.fromResource(file).getLines().toVector.map(_.toLong)

  def mix(newNumber: Long, original: Long): Long = newNumber ^ original
  def prune(number: Long): Long = number % 16777216L

  def nextSecret(secret: Long): Long = {
    val step1 = prune(mix(secret * 64L, secret))
    val step2 = prune(mix(step1 / 32L, step1))
    prune(mix(step2 * 2048L, step2))
  }

  val result = input.map(secret => (0 until 2000).foldLeft(secret) { case (acc, _) => nextSecret(acc) })
  println(result.sum)

  def price(secret: Long): Long = secret % 10L
  val allPrices: Vector[Vector[Long]] = input.map(secret => (0 until 2000).foldLeft((secret, Vector(price(secret)))){ case ((acc, accPrices), _) =>
    val next = nextSecret(acc)
    (next, accPrices :+ price(next))
  }).map(_._2)
  val allDiffs = allPrices.map(prices => prices.sliding(2).map { case Vector(a, b) => b-a }.toVector)

  val allDiffsSliding4: Vector[Vector[Vector[Long]]] = allDiffs.map(_.sliding(4).toVector)
  val allDiffIndices: Vector[Map[Vector[Long], Int]] =
    allDiffsSliding4.map(_.zipWithIndex.groupBy(_._1).map((k, v) => (k, v.map(_._2).min)).toMap)

  val allMonkeyIndices: Map[Vector[Long], Long] =
    allDiffIndices.flatMap(_.keys).toSet.map { seq =>
      (seq, allDiffIndices.zipWithIndex.map{ (monkeyIndex, monkey) =>
        val index = monkeyIndex.getOrElse(seq, -1)
        if index == -1 then 0L else
          allPrices(monkey)(index + 4)
      }.sum)
    }.toMap

  def bananas(sequence: Vector[Long]): Long = {
    allDiffIndices.indices.map{ i =>
      val index = allDiffIndices(i).getOrElse(sequence, -1)
      if index == -1 then 0L else
        val price = allPrices(i)(index+4)
        price
    }.sum
  }

  def bananas2(sequence: Vector[Long]): Long = {
    allMonkeyIndices.getOrElse(sequence, 0L)
  }
//  println(bananas(Vector(-1L, -1L, 0L, 2L)))
//  println(bananas(Vector(-2L, 1L, -1L, 3L)))

  val start = System.currentTimeMillis()
  val result2 = (for {
    a <- -9L until 10L
    b <- -9L until 10L
    c <- -9L until 10L
    d <- -9L until 10L
  } yield {
    bananas2(Vector(a, b, c, d))
  }).max
  println(result2)


