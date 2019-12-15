import scala.io.Source

object Problem12 extends App {

  case class Moon(x: Int, y: Int, z: Int, vx: Int, vy: Int, vz: Int) {
    def energy: Int = (Math.abs(x) + Math.abs(y) + Math.abs(z)) * (Math.abs(vx) + Math.abs(vy) + Math.abs(vz))
  }
  val inputRegex = "<x=([0-9\\-]+), y=([0-9\\-]+), z=([0-9\\-]+)>".r

  val inputMoons = Source.fromResource("12-input.txt").getLines().map {
    case inputRegex(x, y, z) => Moon(x.toInt, y.toInt, z.toInt, 0, 0, 0)
  }.toList

  inputMoons.foreach(println)

  def simulateStep(moons: List[Moon]): List[Moon] = {
    moons.map { moon1 =>
      var newVx = moon1.vx
      var newVy = moon1.vy
      var newVz = moon1.vz
      moons.foreach{ moon2 =>
        if (moon2.x > moon1.x)
          newVx += 1
        else if (moon2.x < moon1.x)
          newVx -= 1
        if (moon2.y > moon1.y)
          newVy += 1
        else if (moon2.y < moon1.y)
          newVy -= 1
        if (moon2.z > moon1.z)
          newVz += 1
        else if (moon2.z < moon1.z)
          newVz -= 1
      }
      moon1.copy(vx = newVx, vy = newVy, vz= newVz)
    }.map(moon => moon.copy(x = moon.x+moon.vx, y = moon.y+moon.vy, z = moon.z+moon.vz))
  }

  val resultingMoons = (1 to 100).foldLeft(inputMoons) { (moons, step) =>
    val result = simulateStep(moons)
    //println(s"Step $step")
    //result.foreach(moon => println(s"${moon.x} ${moon.y} ${moon.z} ${moon.vx} ${moon.vy} ${moon.vz}"))
    result
  }

  val resultingEnergy = resultingMoons.map(_.energy).sum

  println(resultingEnergy)

  val periods = Array.fill(3)(-1)
  var moons = inputMoons
  var steps = 0
  while (periods.contains(-1)) {
    moons = simulateStep(moons)
    steps += 1
    if (periods(0) == -1 && moons.zip(inputMoons).forall(moons => moons._1.x == moons._2.x && moons._1.vx == 0))
      periods(0) = steps
    if (periods(1) == -1 && moons.zip(inputMoons).forall(moons => moons._1.y == moons._2.y && moons._1.vy == 0))
      periods(1) = steps
    if (periods(2) == -1 && moons.zip(inputMoons).forall(moons => moons._1.z == moons._2.z && moons._1.vz == 0))
      periods(2) = steps
//    moons.zipWithIndex.foreach { case (moon, index) =>
//      if (moon.x == inputMoons(index).x && periods(index)(0) == -1 && moon.vx == 0)
//        periods(index)(0) = steps
//      if (moon.y == inputMoons(index).y && periods(index)(1) == -1 && moon.vy == 0)
//        periods(index)(1) = steps
//      if (moon.z == inputMoons(index).z && periods(index)(2) == -1 && moon.vz == 0)
//        periods(index)(2) = steps
//      if (moon == inputMoons(index) && periods(index) == -1) {
//        println(s"Moon $index back to it's previous state after $steps steps")
//        periods(index) = steps
//      }
    }
  println(periods.mkString(" "))

  def gcd(a: Long, b: Long):Long=if (b==0) a.abs else gcd(b, a%b)
  def lcm(a: Long, b: Long): Long=(a*b).abs/gcd(a,b)

//  val moonPeriods = periods.map(moon => lcm(moon(0), lcm(moon(1), moon(2))))

//  println(moonPeriods.mkString(" "))

  val answer = lcm(periods(0), lcm(periods(1), periods(2)))
//
//  println(s"Periods: ${periods.map(_.toLong).reduce((a, b) => lcm(a,b))}")
  println(s"Answer $answer")

}
