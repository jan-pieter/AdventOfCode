import scala.annotation.tailrec
import scala.io.Source

object Problem23 extends App {

  val input = Source.fromResource("23-input.txt").getLines().next()

  case class RingItem(value: Int, prev: Int, next: Int)
  trait Ring {
    def ring: Array[RingItem]
    var current: Int

    def move(): Unit = {
      //Pickup 3 items
      val item1 = ring(ring(current).next)
      val item2 = ring(item1.next)
      val item3 = ring(item2.next)
      ring(item1.prev) = ring(item1.prev).copy(next = item3.next)
      ring(item3.next) = ring(item3.next).copy(prev = item1.prev)

      @tailrec
      def findDestination(start: Int): Int = {
        val newDestination = (start - 1) match {
          case 0 => ring.length - 1
          case other => other
        }
        if (newDestination == item1.value || newDestination == item2.value || newDestination == item3.value)
          findDestination(newDestination)
        else
          newDestination
      }
      val destination = findDestination(current)

      //println(s"destination: $destination")

      val oldRef = ring(destination).next
      if (oldRef == -1) println(s"Aargh $current $destination $item1 $item2 $item3 ${ring(destination)}")
      ring(destination) = ring(destination).copy(next = item1.value)
      ring(item1.value) = ring(item1.value).copy(prev = destination)
      ring(oldRef) = ring(oldRef).copy(prev = item3.value)
      ring(item3.value) = ring(item3.value).copy(next = oldRef)

      current = ring(current).next
      //printRing()
    }
  }
  case class Ring1(in: String) extends Ring {
    val ring: Array[RingItem] = Array.fill(in.length + 1)(RingItem(-1, -1, -1))
    var current: Int = in.head.asDigit

    in.foldLeft(in.last) {
      case (prevChar, newChar) =>
        ring(newChar.asDigit) = ring(newChar.asDigit).copy(value = newChar.asDigit, prev = prevChar.asDigit)
        ring(prevChar.asDigit) = ring(prevChar.asDigit).copy(next = newChar.asDigit)
        newChar
    }

    printRing()

    def printRing(cur: Int = current): Unit = {
      var next = cur
      var item = ring(cur)
      print(item.value)
      next = item.next
      while (next != cur) {
        item = ring(next)
        print(item.value)
        next = item.next
      }
      print("\n")
    }
  }

  case class Ring2(in: String) extends Ring {
    val ring: Array[RingItem] = Array.fill(1000001)(RingItem(-1, -1, -1))
    var current: Int = in.head.asDigit

    in.foldLeft(1000000) {
      case (prev, newChar) =>
        ring(newChar.asDigit) = ring(newChar.asDigit).copy(value = newChar.asDigit, prev = prev)
        ring(prev) = ring(prev).copy(next = newChar.asDigit)
        newChar.asDigit
    }
    ring(in.last.asDigit) = ring(in.last.asDigit).copy(next = 10)

    (10 to 1000000).foreach { cup =>
      ring(cup) = ring(cup).copy(value = cup, prev = cup - 1, next = if(cup == 1000000) current else cup + 1)
    }

    def printSolution(): Unit = {
      val item1 = ring(1)
      println(item1.next)
      val item2 = ring(item1.next)
      println(item2.next)
      println(item1.next.toLong * item2.next.toLong)
    }
  }

  val r = Ring1(input)

  (1 to 100).foreach { i =>
    //println(s"move $i")
    r.move()
  }
  r.printRing(1)

  val r2 = Ring2(input)

  (1 to 10000000).foreach { i =>
    //println(s"move $i")
    r2.move()
  }
  r2.printSolution()

}
