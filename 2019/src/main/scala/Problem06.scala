import scala.io.Source

object Problem06 extends App {

  case class Body(name: String, inOrbit: Vector[String])
  val lines = Source.fromResource("06-input.txt").getLines().map(_.split("\\)")).map(array => array(0) -> array(1)).toVector
  val map: Map[String, Body] = lines.groupBy(_._1).map{ case (name, orbiting) => name -> Body(name, orbiting.map(_._2)) }

  def orbits(key: String, start: Long = 1): Long = {
    map.get(key) match {
      case None => 0
      case Some(Body(name, inOrbit)) if inOrbit.isEmpty => 0
      case Some(Body(name, inOrbit)) => inOrbit.size * start + inOrbit.map(str => orbits(str, start+1)).sum
    }
  }
  println(orbits("COM"))

  val target = map.values.find(_.inOrbit.contains("SAN")).get.name
  val start = map.values.find(_.inOrbit.contains("YOU")).get.name


  def distanceDown(key: String): Option[Long] = {
    map.get(key) match {
      case None => None
      case Some(Body(name, inOrbit)) if inOrbit.contains(target) => Some(0)
      case Some(Body(name, inOrbit)) if inOrbit.isEmpty => None
      case Some(Body(name, inOrbit)) => inOrbit.flatMap(str => distanceDown(str).map(_ + 1)).headOption
    }
  }
  def distanceToTarget(key: String): Long = {
    map(key) match {
      case Body(name, inOrbit) => distanceDown(name).getOrElse(distanceToTarget(map.values.find(_.inOrbit.contains(name)).get.name))+1
    }
  }

  println(distanceToTarget(start))


}
