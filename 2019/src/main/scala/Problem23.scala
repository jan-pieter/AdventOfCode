import IntCodeProcessor2.{Halted, NeedsInput, Result, State}

import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object Problem23 extends App {

  val inputRegister = Source.fromResource("23-input.txt").getLines().next().split(",").map(_.toLong)

  val registers = Vector.fill(50){
    val register = Array.fill(100000)(0L)
    Array.copy(inputRegister, 0, register, 0, inputRegister.length)
    register
  }
  
  val inputs = (0 until 50).map(i => new mutable.Queue[Long]() += i)
  
  inputs.foreach(println)
  
  val processors = (0 until 50).map(i => new IntCodeProcessor2(registers(i))).toVector
  
  var finished = false
  
  var toRun: mutable.Queue[Int] = mutable.Queue(0 until 50 :_*)
  case class Packet(destination: Int, x: Long, y: Long)
  val packetQueue: mutable.Queue[Packet] = mutable.Queue.empty

  def run(index: Int): State = {
    val processor = processors(index)
    val input = inputs(index)
    @scala.annotation.tailrec
    def doRun(in: Option[Long]): State = {
      //println(s"Running processor $index with input $in")
      processor.run(in) match {
        case Result(destination: Long) =>
          val x = processor.run().asInstanceOf[Result[Long]].value
          val y = processor.run().asInstanceOf[Result[Long]].value
          println(s"Sending $x $y to $destination")
          if (destination == 255) {
            println("Done!")
            System.exit(0)
          }
          toRun.enqueue(destination.toInt)
          inputs(destination.toInt).enqueue(x, y)
          Result(Packet(destination.toInt, x, y))
        case NeedsInput if input.nonEmpty => doRun(Some(input.dequeue()))
        case NeedsInput => NeedsInput
        case Halted => Halted
      }
    }
    doRun(if(input.isEmpty) Some(-1L) else Some(input.dequeue()))
  }
  
  while (true) {
    if (toRun.isEmpty)
      toRun = mutable.Queue(0 until 50 :_*)
    val index = toRun.dequeue()

   // println(s"Running processor $index")
    val result = run(index)
    //println(s"Result $result")
    //Thread.sleep(100)
  }

//  def runF(processor: IntCodeProcessor): Future[Unit] = {
//    val f = Future {
//      var done = false
//      while (!done) {
//        val destination = processor.run()
//        val x = processor.run()
//        val y = processor.run()
//        if (destination == 255) {
//          println(s"Sent $x $y to 255")
//          done = true
//        } else {
//          println(s"Sending $x $y to $destination")
//          inputs(destination.toInt).enqueue(x, y)
//        }
//      }
//    }
//    f.onComplete {
//      case Success(value) => println("Completed successfully")
//      case Failure(e) => println(s"Completed with failure ${e.getMessage}")
//    }
//    f
//  }
//
//  val futures = processors.map(runF)
//
//  Await.ready(Future.sequence(futures), 1.minute)

}
