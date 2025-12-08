import scala.collection.mutable
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
  val pairs: Vector[(Box, Box)] = (for {
    box1 <- input
    box2 <- input
    if box1 != box2 && box1.hashCode() < box2.hashCode()
  } yield box1 -> box2).sortBy((box1, box2) => box1.distanceTo(box2))

  val circuits = mutable.Map.empty[Box, Int]
  var nextCircuit = 1
  var connected = 0
  var i = 0
  while (connected < toConnect){
    val pair = pairs(i)
    circuits.get(pair._1) -> circuits.get(pair._2) match {
      case Some(c1) -> Some(c2) if c1 == c2 =>
//        println(s"Doing nothing for $pair") // Do nothing
        connected += 1
      case Some(c1) -> Some(c2) => // Merge circuits
//        println(s"Merging $pair")
        circuits.filter(_._2 == c2).foreach(p => circuits.update(p._1, c1))
        connected += 1
      case Some(c1) -> None => // Connect
//        println(s"Adding2 $pair")
        circuits.addOne(pair._2 -> c1)
        connected += 1
      case None -> Some(c2) => // Connect
//        println(s"Adding1 $pair")
        circuits.addOne(pair._1 -> c2)
        connected += 1
      case None -> None => // New circuit
//        println(s"New for $pair")
        circuits.addOne(pair._1 -> nextCircuit)
        circuits.addOne(pair._2 -> nextCircuit)
        nextCircuit += 1
        connected += 1
    }
    i += 1
  }

  println(circuits.groupBy(_._2).map((cluster, elems) => cluster -> elems.size).toVector.sortBy(_._2).reverse.take(3).map(_._2).product)

  val circuits2 = mutable.Map.empty[Box, Int]
  var nextCircuit2 = 1
  var i2 = 0
  var connectedLast: Option[(Box, Box)] = None
  while (circuits2.size != input.size || circuits2.values.toSet.size != 1) {
    val pair = pairs(i2)
//    println(s"Connecting $pair")
    connectedLast = Some(pair)
    circuits2.get(pair._1) -> circuits2.get(pair._2) match {
      case Some(c1) -> Some(c2) if c1 == c2 =>
        //        println(s"Doing nothing for $pair") // Do nothing
      case Some(c1) -> Some(c2) => // Merge circuits
        //        println(s"Merging $pair")
        circuits2.filter(_._2 == c2).foreach(p => circuits2.update(p._1, c1))
      case Some(c1) -> None => // Connect
        //        println(s"Adding2 $pair")
        circuits2.addOne(pair._2 -> c1)
      case None -> Some(c2) => // Connect
        //        println(s"Adding1 $pair")
        circuits2.addOne(pair._1 -> c2)
      case None -> None => // New circuit
        //        println(s"New for $pair")
        circuits2.addOne(pair._1 -> nextCircuit2)
        circuits2.addOne(pair._2 -> nextCircuit2)
        nextCircuit2 += 1
    }
    i2 += 1
  }

  println(connectedLast.get._1.x.toLong * connectedLast.get._2.x.toLong)





