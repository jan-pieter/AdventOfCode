import scala.collection.mutable
import scala.io.Source

object Problem07 extends App {

  val registerVector = Source.fromResource("07-input.txt").getLines().next().split(",").map(_.toLong).toVector

  val thrusts = for {
    phase1 <- 0 to 4
    phase2 <- 0 to 4 if phase2 != phase1
    phase3 <- 0 to 4 if phase3 != phase1 && phase3 != phase2
    phase4 <- 0 to 4 if phase4 != phase3 && phase4 != phase2 && phase4 != phase1
    phase5 <- 0 to 4 if phase5 != phase4 && phase5 != phase3 && phase5 != phase2 && phase5 != phase1
  } yield {
    val processor1 = new IntCodeProcessor(registerVector.toArray, mutable.Queue(phase1, 0L))
    val result1 = processor1.run()
    //    println(result1)
    val processor2 = new IntCodeProcessor(registerVector.toArray, mutable.Queue(phase2, result1))
    val result2 = processor2.run()
    //    println(result2)
    val processor3 = new IntCodeProcessor(registerVector.toArray, mutable.Queue(phase3, result2))
    val result3 = processor3.run()
    //    println(result3)
    val processor4 = new IntCodeProcessor(registerVector.toArray, mutable.Queue(phase4, result3))
    val result4 = processor4.run()
    //    println(result4)
    val processor5 = new IntCodeProcessor(registerVector.toArray, mutable.Queue(phase5, result4))
    val result5 = processor5.run()
    //    println(result5)
    //println(s"$phase1 $phase2 $phase3 $phase4 $phase5 : $result5")
    result5
  }

  println(thrusts.max)

  val thrusts2 = for {
    phase1 <- 5L to 9L
    phase2 <- 5L to 9L if phase2 != phase1
    phase3 <- 5L to 9L if phase3 != phase1 && phase3 != phase2
    phase4 <- 5L to 9L if phase4 != phase3 && phase4 != phase2 && phase4 != phase1
    phase5 <- 5L to 9L if phase5 != phase4 && phase5 != phase3 && phase5 != phase2 && phase5 != phase1
  } yield {
    var result = 0L
    val (register1, register2, register3, register4, register5) = (registerVector.toArray,registerVector.toArray,registerVector.toArray,registerVector.toArray,registerVector.toArray)
    val (queue1, queue2, queue3, queue4, queue5) = (mutable.Queue(phase1), mutable.Queue(phase2), mutable.Queue(phase3), mutable.Queue(phase4), mutable.Queue(phase5))
    val processor1 = new IntCodeProcessor(register1, queue1)
    val processor2 = new IntCodeProcessor(register2, queue2)
    val processor3 = new IntCodeProcessor(register3, queue3)
    val processor4 = new IntCodeProcessor(register4, queue4)
    val processor5 = new IntCodeProcessor(register5, queue5)
    var done = false
    while(!done) {
      queue1.enqueue(result)
      //println(s"Q1: $queue1 P1: $position1")
      val result1 = processor1.run()
      //println(result1)

      if (result1 == Long.MinValue) {
        //println(result)
        done = true
      } else {
        queue2.enqueue(result1)
        //    println(result1)
        val result2 = processor2.run()
        queue3.enqueue(result2)
        //    println(result2)
        val result3 = processor3.run()
        queue4.enqueue(result3)
        //    println(result3)
        val result4 = processor4.run()
        queue5.enqueue(result4)
        //    println(result4)
        val result5 = processor5.run()
        //    println(result5)
        //println(s"$phase1 $phase2 $phase3 $phase4 $phase5 : $result5")
        result = result5
      }
    }
    result
  }

  println(thrusts2.max)

}
