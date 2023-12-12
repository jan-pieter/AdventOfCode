import scala.collection.mutable
import scala.io.Source

object Problem12 extends App:
  def toGroups(springs: String): Vector[Int] = springs.foldLeft(('-', Vector.empty[Int])) {
    case ((last, soFar), c) if c == last && c == '#' => (last, soFar.updated(soFar.length - 1, soFar(soFar.length - 1) + 1))
    case ((last, soFar), c) if c == '#' => (c, soFar.appended(1))
    case ((last, soFar), c) => (c, soFar)
  }._2
  case class Configuration(springs: String, groups: Vector[Int]) {
    def arrangements: Long = {
      def isPartValid(spr: String): Boolean = {
        val r = toGroups(spr)
        r.length <= 1 || r.dropRight(1) == groups.take(r.length-1)
      }
      def isValid(spr: String): Boolean = {
        val r = toGroups(spr)
        r == groups
      }
      springs.foldLeft(Vector("")) {
        case (arr, '?') => arr.map(_.appended('.')).filter(isPartValid) ++ arr.map(_.appended('#')).filter(isPartValid)
        case (arr, other) => arr.map(_.appended(other)).filter(isPartValid)
      }.count(isValid)
    }
    def unfold: Configuration = Configuration(
      Vector.fill(5)(springs).mkString("?"),
      Vector.fill(5)(groups).flatten
    )
    def arrangements2: Long = {
      val cache = mutable.Map.empty[(String, Vector[Int]), Long]
      def partialArrangements(leftSprings: String, leftGroups: Vector[Int]): Long = {
        val toConsider = leftSprings.dropWhile(_=='.')
        if cache.contains(toConsider -> leftGroups) then
          cache(toConsider -> leftGroups)
        else if toConsider.count(_!= '.') < leftGroups.sum then
          0L
        else if toConsider.length < leftGroups.sum + leftGroups.length - 1 then
          0L
        else
          val r = toConsider.headOption match {
            case None => if leftGroups.isEmpty then 1L else 0L
            case Some('#') if leftGroups.isEmpty => 0L
            case Some('#') if toConsider.take(leftGroups.head).count(_ != '.') == leftGroups.head && (toConsider.length == leftGroups.head || toConsider.drop(leftGroups.head).head == '.')  =>
              partialArrangements(toConsider.drop(leftGroups.head), leftGroups.tail)
            case Some('#') if toConsider.take(leftGroups.head).count(_ != '.') == leftGroups.head && toConsider.drop(leftGroups.head).head == '?' =>
              partialArrangements('.' + toConsider.drop(leftGroups.head+1), leftGroups.tail)
            case Some('#') => 0L
            case Some('?') =>
              partialArrangements(toConsider.tail, leftGroups) +
                partialArrangements('#' + toConsider.tail, leftGroups)
          }
          cache.addOne((toConsider -> leftGroups) -> r)
          r
      }
      val r = partialArrangements(springs, groups)
//      println('.')
      r
    }
  }
  val input = Source.fromResource("12-input.txt").getLines().toVector.map(_.split(" ") match {
    case Array(springs, groups) => Configuration(springs, groups.split(",").toVector.map(_.toInt))
  })

  println(input.map(_.arrangements).sum)
//  println(input.map(_.arrangements2))
//  input.head.arrangements2

//  println(input.map(_.unfold))
  println(input.map(_.unfold.arrangements2).sum)


