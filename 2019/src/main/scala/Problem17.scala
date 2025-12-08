import scala.collection.mutable
import scala.io.Source

object Problem17 extends App {

  val intputRegister = Source.fromResource("17-input.txt").getLines().next().split(",").map(_.toLong)

  val register = Array.fill(1000000)(0L)
  Array.copy(intputRegister, 0, register, 0, intputRegister.length)
  register(0) = 2L

  val instructions = "A,A,B,C,A,C,B,C,A,B\nL,4,L,10,L,6\nL,6,L,4,R,8,R,8\nL,6,R,8,L,10,L,8,L,8\nn\n"
  println(instructions.map(_.toLong).mkString(", "))
  println('\n'.toLong)
  val input = mutable.Queue[Long](instructions.map(_.toLong)*)
  val processor = new IntCodeProcessor(register, input)

  val world = Array.fill(50,50)(' ')
  var x,y = 0

  var output = -1L
  while (output != Long.MinValue) {
    output = processor.run()
    output match {
      case Long.MinValue => //no-op
      case 10L =>
        print("\n")
//        println(world(y).mkString)
        x = 0
        y += 1
      case other if other > 128 =>
        println(s"Output: $output")
      case other =>
//        world(y)(x) = other.toChar
//        print()
//        x += 1
//      case other =>
        print(s"${other.toChar}")
    }
  }

//  world.foreach(line => println(line.mkString))
//
//  val result = (for {
//    posX <- world(0).indices if posX > 0 && posX < world(0).length - 2
//    posY <- world.indices if posY > 0 && posY < world.length
//  } yield {
//    if (world(posY)(posX) == '#' && world(posY-1)(posX) == '#' && world(posY+1)(posX) == '#' && world(posY)(posX-1) == '#' && world(posY)(posX+1) == '#')
//      posX * posY
//    else
//      0
//  }).sum
//
//  println(s"Alignmentparameter sum: $result")

  //L,4,L,10,L,6,L,4,L,10,L,6,L,6,L,4,R,8,R,8,L,6,R,8,L,10,L,8,L,8,L,4,L,10,L,6,L,6,R,8,L,10,L,8,L,8,L,6,L,4,R,8,R,8,L,6,R,8,L,10,L,8,L,8,L,4,L,10,L,6,L,6,L,4,R,8,R,8
  //A,A,A,AA,A,A,A,A,A,AA,A,A,B,B,B,B,B,B,B,B,C,C,C,C,C,CC,C,C,C,C,A,A,A,AA,A,A,C,C,C,C,C,CC,C,C,C,C,B,B,B,B,B,B,B,B,C,C,C,C,C,CC,C,C,C,C,A,A,A,AA,A,A,B,B,B,B,B,B,B,B

//  'A',',','A',',','B',',','C',',','A',',','C',',','B',',','C',',','A',',','B',10.toChar,'L',',','4',',','L',',',
  //A,A,B,C,A,C,B,C,A,B
  //A: L,4,L,10,L,6
  //B: L,6,L,4,R,8,R,8
  //c: L,6,R,8,L,10,L,8,L,8

}
