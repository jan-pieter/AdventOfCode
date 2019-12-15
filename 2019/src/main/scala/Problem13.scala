import scala.collection.mutable
import scala.io.Source

object Problem13 extends App {

  val intputRegister = Source.fromResource("13-input.txt").getLines().next().split(",").map(_.toLong)

  val register = Array.fill(1000000)(0L)
  Array.copy(intputRegister, 0, register, 0, intputRegister.length)
  register(0) = 2
  val input = mutable.Queue[Long]()

  val processor = new IntCodeProcessor(register, input)

  var score = -1L
//  var maxX, maxY = 0L
  var step = 0L
  val board = Array.fill(24, 41)(0)
  var ballX = -1L
  var ballY = -1L
  var prevBallX = -1L
  var prevBallY = -1L
  var ballAtPaddle = -1L
  var paddleX = -1L

  var done = false
  while (!done) {
    val x = processor.run()
//    maxX = Math.max(x, maxX)
    if (x == Long.MinValue) {
      done = true
    } else {
      val y = processor.run()
//      maxY = Math.max(y, maxY)
      val value = processor.run()
      if (x == -1 && y == 0) {
        println(s"partial score $value")
        board.foreach(line => println(line.mkString))
        score = value
      } else {
//        println(s"$x $y $value")
        board(y.toInt)(x.toInt) = value.toInt
        if (value == 3L) {
          paddleX = x
          //println(s"Paddle at $x $y Ball at $ballX $ballY previously at $prevBallX $prevBallY")
        } else if (value == 4L) {
          prevBallX = ballX
          prevBallY = ballY
          ballX = x
          ballY = y
//          if (prevBallX != -1 && prevBallY != -1) {
//            var directionY = ballY - prevBallY
//            var directionX = ballX - prevBallX
//            var newBallX = ballX
//            var newBallY = ballY
//            while (newBallY < 22) {
//              newBallY += directionY
//              newBallX += directionX
//              if (newBallX <= 0) {
//                newBallX = Math.abs(newBallX)
//                directionX = directionX * -1
//              } else if (newBallX >= 40) {
//                newBallX = (newBallX - 40) * -1
//                directionX = directionX * -1
//              }
//              if (newBallY != 22 && board(newBallY.toInt + directionY.toInt)(newBallX.toInt) != 0L) {
//                directionY = directionY * -1
//              }
//            }
//            ballAtPaddle = newBallX
//            println(s"Ball will hit at $newBallX $newBallY")
//          }
//
//          if (paddleX < ballAtPaddle) {
//            input.enqueue(1L)
//            paddleX += 1
//          } else if (paddleX > ballAtPaddle) {
//            input.enqueue(-1L)
//            paddleX -= 1
//          } else {
//            input.enqueue(0L)
//          }
          println(s"PaddleX $paddleX Ball at $ballX $ballY previously at $prevBallX $prevBallY")
          if (paddleX < ballX) {
            input.enqueue(1L)
            paddleX += 1
            println("Input 1")
          } else if (paddleX > ballX) {
            input.enqueue(-1L)
            paddleX -= 1
            println("Input -1")
          } else if (ballY < 21) {
            input.enqueue(ballX - prevBallX)
            paddleX += (ballX - prevBallX)
            println(s"Input calc1 ${ballX - prevBallX}")
          } else if (ballY == 21) {
            input.enqueue(0L)
            println(s"Input calc0 0")
          } else {
            input.enqueue((ballX - prevBallX) * -1)
            paddleX += (ballX - prevBallX) * -1
            println(s"Input calc2 ${(ballX - prevBallX) * -1}")
          }
        }
      }
    }
    step += 1
  }
  //println(s"Block tiles ${board.map(_.count(_ == 2)).sum} $maxX $maxY ${step-1}")
  println(s"Score $score after $step steps")


}
