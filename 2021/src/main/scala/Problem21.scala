import scala.collection.mutable

object Problem21 extends App {
//  val (inputPos1, inputPos2) = (4-1, 8-1)
  val (inputPos1, inputPos2) = (8-1, 10-1)
  var (pos1, pos2) = (inputPos1, inputPos2)

  var (score1, score2) = (0,0)

  var deterministicDieState = 0L
  var timesRolled = 0L
  def rollDeterministicDie(): Int = {
    val result = (deterministicDieState % 100).toInt + 1
    deterministicDieState = deterministicDieState + 1L
    timesRolled = timesRolled + 1
    result
  }

  var turn1 = true
  while (score1 < 1000 && score2 < 1000) {
    val (throw1, throw2, throw3) = (rollDeterministicDie(), rollDeterministicDie(), rollDeterministicDie())
    if (turn1) {
      pos1 = (pos1 + throw1 + throw2 + throw3) % 10
      score1 = score1 + pos1 + 1
    } else {
      pos2 = (pos2 + throw1 + throw2 + throw3) % 10
      score2 = score2 + pos2 + 1
    }
    turn1 = !turn1
  }

  val answer1 = if (score1 >= 1000) {
    score2 * timesRolled
  } else {
    score1 * timesRolled
  }
  println(s"Answer1: $answer1")

  val cache: mutable.Map[(Int, Int, Int, Int), (Long, Long)] = mutable.Map.empty

  def wins(pos1: Int, pos2: Int, score1: Int, score2: Int): (Long, Long) = {
    cache.getOrElseUpdate((pos1, pos2, score1, score2), {
      // p1
      (for {
        roll1 <- 1 to 3
        roll2 <- 1 to 3
        roll3 <- 1 to 3
      } yield {
        val newPos1 = (pos1 + roll1 + roll2 + roll3) % 10
        val newScore1 = score1 + newPos1 + 1
//        println(s"$roll1 $roll2 $roll3 $newPos1 $newScore1")
        if (newScore1 >= 21) {
          (1L, 0L)
        } else {
//          println(s"Roll2")
          // p2
          (for {
            roll4 <- 1 to 3
            roll5 <- 1 to 3
            roll6 <- 1 to 3
          } yield {
            val newPos2 = (pos2 + roll4 + roll5 + roll6) % 10
            val newScore2 = score2 + newPos2 + 1
            if (newScore2 >= 21) {
              (0L, 1L)
            } else {
              wins(newPos1, newPos2, newScore1, newScore2)
            }
          }).reduce[(Long, Long)]{
            case (t1, t2) => (t1._1 + t2._1, t1._2 + t2._2)
          }
        }
      }).reduce[(Long, Long)]{
        case (t1, t2) => (t1._1 + t2._1, t1._2 + t2._2)
      }
    })
  }

  val answer2 = wins(inputPos1, inputPos2, 0, 0)
  println(s"Answer2: $answer2 max: ${answer2._1.max(answer2._2)}")
}
