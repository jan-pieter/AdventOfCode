import scala.collection.mutable
import scala.io.Source

object Problem22 extends App {

  val input = Source.fromResource("22-input.txt").getLines().toList

  def split(list: List[String]): List[List[String]] = {
    val result = list.foldLeft(List(List.empty[String])) {
      case (state, "") => List.empty[String] :: state
      case (state, input) => (input :: state.head) :: state.tail
    }
    result.reverse.map(_.reverse)
  }

  val splittedInput = split(input)

  val deck1: mutable.Queue[Int] = mutable.Queue(splittedInput(0).filterNot(_.isEmpty).drop(1).map(_.toInt): _*)
  val deck2: mutable.Queue[Int] = mutable.Queue(splittedInput(1).filterNot(_.isEmpty).drop(1).map(_.toInt): _*)

  def combat(input1: mutable.Queue[Int], input2: mutable.Queue[Int]): mutable.Queue[Int] = {
    val deck1 = input1.clone()
    val deck2 = input2.clone()
    while (deck1.nonEmpty && deck2.nonEmpty) {
      val card1 = deck1.dequeue()
      val card2 = deck2.dequeue()
      if (card1 > card2) {
        deck1.enqueue(card1, card2)
      } else {
        deck2.enqueue(card2, card1)
      }
    }
    if (deck1.nonEmpty) deck1 else deck2
  }

  def score(deck: mutable.Queue[Int]): Int = {
    deck.reverse.zipWithIndex.map {
      case (card, index) => card * (index + 1)
    }.sum
  }

  //println(score(combat(deck1, deck2)))

  def recursiveCombat(input1: mutable.Queue[Int], input2: mutable.Queue[Int]): (mutable.Queue[Int], mutable.Queue[Int]) = {
    //println("New game")
    val deck1 = input1.clone()
    val deck2 = input2.clone()
    val states: mutable.Set[String] = mutable.Set.empty
    while (deck1.nonEmpty && deck2.nonEmpty) {
      val stateStr = s"${deck1.mkString(",")} ${deck2.mkString(",")}"
      if (states.contains(stateStr)) {
        deck1.enqueue(deck2.dequeueAll(_ => true): _*)
      } else {
        states.add(stateStr)
        val card1 = deck1.dequeue()
        val card2 = deck2.dequeue()
        if (deck1.size >= card1 && deck2.size >= card2) {
          val (resultDeck1, _) = recursiveCombat(deck1.take(card1), deck2.take(card2))
          if (resultDeck1.nonEmpty) {
            deck1.enqueue(card1, card2)
          } else {
            deck2.enqueue(card2, card1)
          }
        } else {
          if (card1 > card2) {
            deck1.enqueue(card1, card2)
          } else {
            deck2.enqueue(card2, card1)
          }
        }
        //println(s"deck1: $deck1")
        //println(s"deck2: $deck2")
      }
    }
    //println("End game")
    deck1 -> deck2
  }

  val (recursiveDeck1, recursiveDeck2) = recursiveCombat(deck1, deck2)
  println(recursiveDeck1)
  println(recursiveDeck2)
  println(score(recursiveDeck1))
  println(score(recursiveDeck2))

}
