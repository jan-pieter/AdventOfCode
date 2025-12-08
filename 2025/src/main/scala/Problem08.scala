import scala.io.Source

object Problem08 extends App:
  case class Box(x: Int, y: Int, z: Int):
    def distanceTo(other: Box): Double = Math.sqrt(Math.pow(x - other.x, 2) + Math.pow(y - other.y, 2) + Math.pow(z - other.z, 2))
//  val (file, toConnect) = ("08-test.txt", 10)
  val (file, toConnect) = ("08-input.txt", 1000)
  val input = Source.fromResource(file).getLines().toVector.map{
    case s"$x,$y,$z" => Box(x.toInt, y.toInt, z.toInt)
  }
//  println(input)
  private val pairs: Vector[(Box, Box)] = (for {
    box1 <- input
    box2 <- input
    if box1 != box2 && box1.hashCode() < box2.hashCode()
  } yield box1 -> box2).sortBy((box1, box2) => box1.distanceTo(box2))

  private val circuits = pairs.take(toConnect).foldLeft(Map.empty[Box, Int]){ (circuits, pair) =>
    circuits.get(pair._1) -> circuits.get(pair._2) match {
      case Some(c1) -> Some(c2) if c1 == c2 => circuits // Do nothing
      case Some(c1) -> Some(c2) => // Merge circuits
        circuits.filterNot(_._2 == c2) ++ circuits.filter(_._2 == c2).map((key, value) => key -> c1)
      case Some(c1) -> None => // Connect
        circuits + (pair._2 -> c1)
      case None -> Some(c2) => // Connect
        circuits + (pair._1 -> c2)
      case None -> None => // New circuit
        val nextCircuit = circuits.values.maxOption.getOrElse(0) + 1
        circuits + (pair._1 -> nextCircuit) + (pair._2 -> nextCircuit)
    }
  }
  println(circuits.groupBy(_._2).map((cluster, elems) => cluster -> elems.size).toVector.sortBy(_._2).reverse.take(3).map(_._2).product)

  private val (_, connectedLast) = pairs.foldLeft((Map.empty[Box, Int], Option.empty[(Box, Box)])) { case ((circuits, last), pair) =>
    circuits.get(pair._1) -> circuits.get(pair._2) match {
      case Some(c1) -> Some(c2) if c1 == c2 => circuits -> last // Do nothing
      case Some(c1) -> Some(c2) => // Merge circuits
        circuits.filterNot(_._2 == c2) ++ circuits.filter(_._2 == c2).map((key, value) => key -> c1) -> Some(pair)
      case Some(c1) -> None => // Connect
        (circuits + (pair._2 -> c1)) -> Some(pair)
      case None -> Some(c2) => // Connect
        (circuits + (pair._1 -> c2)) -> Some(pair)
      case None -> None => // New circuit
        val nextCircuit = circuits.values.maxOption.getOrElse(0) + 1
        (circuits + (pair._1 -> nextCircuit) + (pair._2 -> nextCircuit)) -> Some(pair)
    }
  }

  println(connectedLast.get._1.x.toLong * connectedLast.get._2.x.toLong)
