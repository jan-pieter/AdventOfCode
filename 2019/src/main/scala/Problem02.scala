import scala.io.Source

object Problem02 extends App {

  for {
    noun <- 0 to 99
    verb <- 0 to 99
  } yield {

    val register = Source.fromResource("02-input.txt").getLines().take(1).map(_.split(",").map(_.toLong)).next()
    register(1) = noun
    register(2) = verb

    new IntCodeProcessor(register).run()
    if (register(0) == 19690720)
      println(s"Noun $noun verb $verb solution ${100*noun+verb}")

    //  val register = Array(1,9,10,3,2,3,11,0,99,30,40,50)
    //val register = Array(1,1,1,4,99,5,6,0,99)

//    var position = 0
//    var done = false
//    while (!done) {
//      register(position) match {
//        case 1 =>
//          register(register(position + 3)) = register(register(position + 1)) + register(register(position + 2))
//          position += 4
//        case 2 =>
//          register(register(position + 3)) = register(register(position + 1)) * register(register(position + 2))
//          position += 4
//        case 99 => done = true
//      }
//    }
//
//    if (register(0) == 19690720)
//      println(s"Noun $noun verb $verb solution ${100*noun+verb}")
  }
}
