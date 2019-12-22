import scala.io.Source

object Problem22 extends App {

  val cards = 10007
  val deck = (0 until cards).toVector
  
  trait Operation {
    def shuffle(deck: Vector[Int]): Vector[Int]
  }
  case object DealIntoNewStack extends Operation {
    override def shuffle(deck: Vector[Int]): Vector[Int] = deck.reverse
  }
  case class Cut(n: Int) extends Operation {
    override def shuffle(deck: Vector[Int]): Vector[Int] = if (n < 0) {
      deck.takeRight(n * -1) ++ deck.dropRight(n * -1)
    } else {
      deck.drop(n) ++ deck.take(n)
    }
  }
  case class DealWithIncrement(n: Int) extends Operation {
    override def shuffle(deck: Vector[Int]): Vector[Int] = deck.zipWithIndex.map {
      case (card, index) => (card, (index * n) % cards) 
    }.sortBy(_._2).map(_._1)
  }
  object Operation {
    def apply(string: String): Operation = string match {
      case "deal into new stack" => DealIntoNewStack
      case str if str.startsWith("cut") =>
        val parts = str.split(" ")
        Cut(parts(1).toInt)
      case str if str.startsWith("deal with increment") =>
        val parts = str.split(" ")
        DealWithIncrement(parts(3).toInt)
    }
  }

  val operations = Source.fromResource("22-input.txt").getLines().map(Operation.apply).toVector

  val result = operations.foldLeft(deck)((deckSoFar, operation) => operation.shuffle(deckSoFar))
  
//  println(result)
  
  println(s"Card 2019 at ${result.indexOf(2019)}")

}
