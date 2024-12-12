import scala.io.Source

object Problem12 extends App:
//  val file = "12-test.txt"
  val file = "12-input.txt"
  val input = Source.fromResource(file).getLines().toVector.map(_.toVector)
  case class Region(plant: Char, plots: Set[(Int, Int)], perimeter: Long) {
    def price: Long = plots.size * perimeter
    def price2: Long = plots.size * sides
    def sides: Long = plots.toVector.map{ (y, x) =>
      val isTopLeft = (!plots.contains((y-1, x)) && !plots.contains((y, x-1))) || (plots.contains((y-1, x)) && plots.contains((y, x-1)) && !plots.contains((y-1, x-1)))
      val isTopRight = (!plots.contains((y-1, x)) && !plots.contains((y, x+1))) || (plots.contains((y-1, x)) && plots.contains((y, x+1)) && !plots.contains((y-1, x+1)))
      val isBottomLeft = (!plots.contains((y+1, x)) && !plots.contains((y, x-1))) || (plots.contains((y+1, x)) && plots.contains((y, x-1)) && !plots.contains((y+1, x-1)))
      val isBottomRight = (!plots.contains((y+1, x)) && !plots.contains((y, x+1))) || (plots.contains((y+1, x)) && plots.contains((y, x+1)) && !plots.contains((y+1, x+1)))
      Vector(isTopLeft, isTopRight, isBottomLeft, isBottomRight).count(identity)
    }.sum
  }

  val regions = (for {
    y <- input.indices
    x <- input(y).indices
  } yield (y, x)).foldLeft(Vector.empty[Region]) { case (regions, (y, x)) =>
    if regions.forall(!_.plots.contains((y, x))) then
      @annotation.tailrec
      def fillRegion(toConsider: Set[(Int, Int)], regionSoFar: Region): Region =
        if toConsider.isEmpty then regionSoFar
        else
          val (y, x) = toConsider.head
          if input(y)(x) != regionSoFar.plant then fillRegion(toConsider.tail, regionSoFar)
          else
            val newPlots = regionSoFar.plots + ((y, x))
            val newPerimeter = regionSoFar.perimeter + List((y-1, x), (y+1, x), (y, x-1), (y, x+1)).count((y, x) =>
              y < 0 || y >= input.length || x < 0 || x >= input(y).length || input(y)(x) != regionSoFar.plant
            )
            val newToConsider = List((y-1, x), (y+1, x), (y, x-1), (y, x+1)).filter((y, x) =>
              y >= 0 && y < input.length && x >= 0 && x < input(y).length && !newPlots.contains((y, x))
            )
            fillRegion(newToConsider.toSet ++ toConsider.tail, Region(regionSoFar.plant, newPlots, newPerimeter))
      regions :+ fillRegion(Set((y, x)), Region(input(y)(x), Set.empty, 0))
    else regions
  }

  println(regions.map(_.price).sum)
  println(regions.map(_.price2).sum)
//  println(regions)
//  println(regions.map(_.sides))