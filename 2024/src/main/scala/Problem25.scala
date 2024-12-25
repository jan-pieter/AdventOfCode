import scala.io.Source

object Problem25 extends App:
//  val file = "25-test.txt"
  val file = "25-input.txt"
  val input = Source.fromResource(file).getLines().toVector
  val (locks, keys) = input.foldLeft(Vector.empty[Vector[String]], Vector.empty[Vector[String]]) {
    case (acc, line) if line.isEmpty => acc
    case ((locks, keys), line) if locks.headOption.exists(_.length != 7) => (locks.head.appended(line) +: locks.tail, keys)
    case ((locks, keys), line) if keys.headOption.exists(_.length != 7) => (locks, keys.head.appended(line) +: keys.tail)
    case ((locks, keys), line) if line.exists(_ == '#') => (Vector(line) +: locks, keys)
    case ((locks, keys), line) => (locks, Vector(line) +: keys)
  }
  
  def toHeights(lock: Vector[String]): Vector[Int] =
    lock.tail.transpose.map(_.takeWhile(_ == '#').length)
  
  val (lockHeights, keyHeights) = (locks.map(toHeights), keys.map(key => toHeights(key.reverse)))
  
  def fits(lock: Vector[Int], key: Vector[Int]): Boolean =
    lock.zip(key).forall((lock, key) => lock + key < 6)

  val result = for {
    lock <- lockHeights
    key <- keyHeights
    if fits(lock, key)
  } yield key -> lock
  println(result.size)