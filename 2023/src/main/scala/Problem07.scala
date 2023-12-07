import scala.io.Source

object Problem07 extends App:
  val strengths: String = "AKQJT98765432"
  val strengths2: String = "AKQT98765432J"
  sealed trait HandType extends Ordered[HandType] {
    def order: Int
    def groups: Vector[Char]
    /* Implementation for _real_ poker:
    def compare(that: HandType): Int = if that.order == order then
      groups
        .zip(that.groups)
        .dropWhile((c1, c2) => strengths.indexOf(c1).compare(strengths.indexOf(c2)) == 0)
        .headOption
        .map((c1, c2) => strengths.indexOf(c1).compare(strengths.indexOf(c2)))
        .getOrElse(0)
    else order.compare(that.order)*/
    def compare(that: HandType): Int = order.compare(that.order)
  }
  case class FiveOfAKind(groups: Vector[Char], order: Int = 1) extends HandType
  case class FourOfAKind(groups: Vector[Char], order: Int = 2) extends HandType
  case class FullHouse(groups: Vector[Char], order: Int = 3) extends HandType
  case class ThreeOfAKind(groups: Vector[Char], order: Int = 4) extends HandType
  case class TwoPair(groups: Vector[Char], order: Int = 5) extends HandType
  case class OnePair(groups: Vector[Char], order: Int = 6) extends HandType
  case class HighCard(groups: Vector[Char], order: Int = 7) extends HandType

  object HandType {
    def fromCards(cards: String): HandType = {
      val splitted = cards.groupBy(identity).toVector.sortBy(_._2.length).reverse
      if (splitted.head._2.length == 5) {
        FiveOfAKind(splitted.map(_._1))
      } else if (splitted.head._2.length == 4) {
        FourOfAKind(splitted.map(_._1))
      } else if (splitted.head._2.length == 3 && splitted(1)._2.length == 2) {
        FullHouse(splitted.map(_._1))
      } else if (splitted.head._2.length == 3) {
        ThreeOfAKind(splitted.map(_._1).drop(1).sortBy(c => strengths.indexOf(c)).prepended(splitted.map(_._1).head))
      } else if (splitted.head._2.length == 2 && splitted(1)._2.length == 2) {
        TwoPair(splitted.map(_._1).take(2).sortBy(c => strengths.indexOf(c)).appended(splitted.map(_._1)(2)))
      } else if (splitted.head._2.length == 2) {
        OnePair(splitted.map(_._1).drop(1).sortBy(c => strengths.indexOf(c)).prepended(splitted.map(_._1).head))
      } else {
        HighCard(splitted.map(_._1).sortBy(c => strengths.indexOf(c)))
      }
    }

    def fromCards2(cards: String): HandType = {
      val splitted = cards.filterNot(_ == 'J').groupBy(identity).toVector.sortBy(_._2.length).reverse
      val jokers = cards.count(_ == 'J')
      if (splitted.isEmpty || splitted.head._2.length == 5 || (splitted.head._2.length + jokers) == 5) {
        FiveOfAKind(Vector.empty)
      } else if (splitted.head._2.length == 4 || (splitted.head._2.length + jokers) == 4) {
        FourOfAKind(Vector.empty)
      } else if ((splitted.head._2.length == 3 || (splitted.head._2.length + jokers) == 3) && splitted(1)._2.length == 2) {
        FullHouse(Vector.empty)
      } else if (splitted.head._2.length == 3 || (splitted.head._2.length + jokers) == 3) {
        ThreeOfAKind(Vector.empty)
      } else if (splitted.head._2.length == 2 && splitted(1)._2.length == 2) {
        TwoPair(Vector.empty)
      } else if (splitted.head._2.length == 2 || (splitted.head._2.length + jokers) == 2) {
        OnePair(Vector.empty)
      } else {
        HighCard(Vector.empty)
      }
    }
  }

  case class Hand(cards: String, bid: Long) {
    val handType: HandType = HandType.fromCards(cards)
    val handType2: HandType = HandType.fromCards2(cards)
  }
  val input = Source.fromResource("07-input.txt").getLines().toVector.map(_.split(" ") match {
    case Array(cards, bid) => Hand(cards, bid.toLong)
  })
  val lessThan: (Hand, Hand) => Boolean = (hand1, hand2) => if hand1.handType.compare(hand2.handType) != 0 then {
    hand1.handType.compare(hand2.handType) == -1
  } else {
    hand1.cards.zip(hand2.cards).dropWhile((c1, c2) => strengths.indexOf(c1).compare(strengths.indexOf(c2)) == 0)
      .head match {
      case (c1, c2) => strengths.indexOf(c1) < strengths.indexOf(c2)
    }
  }
  val result = input.sortWith(lessThan).reverse.zipWithIndex.map((hand, index) => hand.bid * (index+1))
  input.sortWith(lessThan).foreach(h => println(h.toString + " type: " + h.handType))
  println(result.sum)

  val lessThan2: (Hand, Hand) => Boolean = (hand1, hand2) => if hand1.handType2.compare(hand2.handType2) != 0 then {
    hand1.handType2.compare(hand2.handType2) == -1
  } else {
    hand1.cards.zip(hand2.cards).dropWhile((c1, c2) => strengths2.indexOf(c1).compare(strengths2.indexOf(c2)) == 0)
      .head match {
      case (c1, c2) => strengths2.indexOf(c1) < strengths2.indexOf(c2)
    }
  }
  val result2 = input.sortWith(lessThan2).reverse.zipWithIndex.map((hand, index) => hand.bid * (index + 1))
  input.sortWith(lessThan2).foreach(h => println(h.toString + " type: " + h.handType2))
  println(result2.sum)
