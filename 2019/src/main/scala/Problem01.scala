import scala.io.Source

object Problem01 extends App {

//  val masses = Vector(12, 14, 1969, 100756)
  val masses = Source.fromResource("01-input.txt").getLines().map(_.toInt).toVector

  def fuel(mass: Int) : Int = (mass / 3) - 2

  def recFuel(mass: Int): Int = mass match {
    case 0 => 0
    case n if fuel(n) <= 0 => 0
    case n =>
      val f = fuel(n)
      f + recFuel(f)
  }

  //val fuels = masses.map(_ / 3).map(_ - 2)

  //println(fuels.sum)

  println(masses.map(recFuel).sum)




}
