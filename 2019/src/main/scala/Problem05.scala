import scala.io.Source
import scala.collection.mutable

object Problem05 extends App {

  val register = Source.fromResource("05-input.txt").getLines().take(1).map(_.split(",").map(_.toLong)).next()
  //val register = Array(3,3,1107,-1,8,3,4,3,99)
  val input = mutable.Queue(5L)
  //val input = mutable.Queue(7)

  val processor = new IntCodeProcessor(register, input)
  var result = 0L
  while (result == 0) {
    result = processor.run()
  }
  println(result)

//  println(s"Output: $result")

//  var position = 0
//  var done = false
//  while (!done) {
//    register(position) match {
//      case 1 =>
//        register(register(position + 3)) = register(register(position + 1)) + register(register(position + 2))
//        position += 4
//      case 101 =>
//        register(register(position + 3)) = register(position + 1) + register(register(position + 2))
//        position += 4
//      case 1001 =>
//        register(register(position + 3)) = register(register(position + 1)) + register(position + 2)
//        position += 4
//      case 1101 =>
//        register(register(position + 3)) = register(position + 1) + register(position + 2)
//        position += 4
//      case 2 =>
//        register(register(position + 3)) = register(register(position + 1)) * register(register(position + 2))
//        position += 4
//      case 102 =>
//        register(register(position + 3)) = register(position + 1) * register(register(position + 2))
//        position += 4
//      case 1002 =>
//        register(register(position + 3)) = register(register(position + 1)) * register(position + 2)
//        position += 4
//      case 1102 =>
//        register(register(position + 3)) = register(position + 1) * register(position + 2)
//        position += 4
//      case 3 =>
//        register(register(position + 1)) = input.dequeue()
//        position += 2
//      case 4 =>
//        println(s"Output: ${register(register(position + 1))}")
//        position += 2
//      case 104 =>
//        println(s"Output: ${register(position + 1)}")
//        position += 2
//      case 5 =>
//        if (register(register(position + 1)) != 0)
//          position = register(register(position + 2))
//        else
//          position += 3
//      case 105 =>
//        if (register(position + 1) != 0)
//          position = register(register(position + 2))
//        else
//          position += 3
//      case 1005 =>
//        if (register(register(position + 1)) != 0)
//          position = register(position + 2)
//        else
//          position += 3
//      case 1105 =>
//        if (register(position + 1) != 0)
//          position = register(position + 2)
//        else
//          position += 3
//      case 6 =>
//        if (register(register(position + 1)) == 0)
//          position = register(register(position + 2))
//        else
//          position += 3
//      case 106 =>
//        if (register(position + 1) == 0)
//          position = register(register(position + 2))
//        else
//          position += 3
//      case 1006 =>
//        if (register(register(position + 1)) == 0)
//          position = register(position + 2)
//        else
//          position += 3
//      case 1106 =>
//        if (register(position + 1) == 0)
//          position = register(position + 2)
//        else
//          position += 3
//      case 7 =>
//        if (register(register(position + 1)) < register(register(position + 2)))
//          register(register(position + 3)) = 1
//        else
//          register(register(position + 3)) = 0
//        position += 4
//      case 107 =>
//        if (register(position + 1) < register(register(position + 2)))
//          register(register(position + 3)) = 1
//        else
//          register(register(position + 3)) = 0
//        position += 4
//      case 1007 =>
//        if (register(register(position + 1)) < register(position + 2))
//          register(register(position + 3)) = 1
//        else
//          register(register(position + 3)) = 0
//        position += 4
//      case 1107 =>
//        if (register(position + 1) < register(position + 2))
//          register(register(position + 3)) = 1
//        else
//          register(register(position + 3)) = 0
//        position += 4
//      case 8 =>
//        if (register(register(position + 1)) == register(register(position + 2)))
//          register(register(position + 3)) = 1
//        else
//          register(register(position + 3)) = 0
//        position += 4
//      case 108 =>
//        if (register(position + 1) == register(register(position + 2)))
//          register(register(position + 3)) = 1
//        else
//          register(register(position + 3)) = 0
//        position += 4
//      case 1008 =>
//        if (register(register(position + 1)) == register(position + 2))
//          register(register(position + 3)) = 1
//        else
//          register(register(position + 3)) = 0
//        position += 4
//      case 1108 =>
//        if (register(position + 1) == register(position + 2))
//          register(register(position + 3)) = 1
//        else
//          register(register(position + 3)) = 0
//        position += 4
//      case 99 => done = true
//    }
//  }
}
