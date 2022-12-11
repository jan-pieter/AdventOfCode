import scala.io.Source

object Problem11 extends App:
  case class Monkey(items: Vector[Long], op: Long => Long, test: Long => Boolean, trueTarget: Int, falseTarget: Int, business: Long, div: Int)
  val monkeys = Source.fromResource("11-input.txt").getLines().toVector.filterNot(_.isEmpty).grouped(6).map {
    case Vector(s"Monkey $no:", s"  Starting items: $items", s"  Operation: new = old $op $i", s"  Test: divisible by $div", s"    If true: throw to monkey $trueTarget", s"    If false: throw to monkey $falseTarget") =>
      Monkey(
        items.split(", ").map(_.toLong).toVector,
        (op, i) match {
          case ("*", "old") => x => x * x
          case ("*", i) => x => x * i.toLong
          case ("+", "old") => x => x + x
          case ("+", i) => x => x + i.toLong
        },
        x => x % div.toLong == 0,
        trueTarget.toInt,
        falseTarget.toInt,
        0L,
        div.toInt
      )
  }.toVector

//  println(monkeys)

  val after20 = (0 until 20).foldLeft(monkeys){ (monkeys, _) =>
//    println()
    val newMonkeys = monkeys.indices.foldLeft(monkeys) { (monkeys, index) =>
      val monkey = monkeys(index)
      val newMonkeys = monkey.items.foldLeft(monkeys) { (monkeys, item) =>
        val newItem = monkey.op(item) / 3
        val target = if (monkey.test(newItem)) monkey.trueTarget else monkey.falseTarget
        monkeys.updated(target, monkeys(target).copy(items = monkeys(target).items :+ newItem))
      }
      newMonkeys.updated(index, monkey.copy(items = Vector.empty, business = monkey.business + monkey.items.length))
    }
//    newMonkeys.foreach(monkey => println(monkey.items))
    newMonkeys
  }

  val solution1 = after20.map(_.business).sorted.takeRight(2).product
  println(solution1)

  val divisions = monkeys.map(_.div).distinct
  case class Monkey2(itemRemainders: Vector[Vector[(Int, Long)]], op: String, rightHand: String, trueTarget: Int, falseTarget: Int, business: Long, div: Int) {
    def applyOp(num: Long, mod: Int): Long = (op, rightHand) match {
      case ("*", "old") => (num * num) % mod
      case ("*", x) => (num * x.toLong) % mod
      case ("+", x) => (num + x.toLong) % mod
    }
    def test(remainders: Vector[(Int, Long)]): Boolean = {
      remainders.find(_._1 == div).getOrElse(sys.error(s"Could not find ${div} in $remainders"))._2 == 0
    }
  }

  val monkeys2 = Source.fromResource("11-input.txt").getLines().toVector.filterNot(_.isEmpty).grouped(6).map {
    case Vector(s"Monkey $no:", s"  Starting items: $items", s"  Operation: new = old $op $i", s"  Test: divisible by $div", s"    If true: throw to monkey $trueTarget", s"    If false: throw to monkey $falseTarget") =>
      Monkey2(
        items.split(", ").map(_.toLong).toVector.map(item => divisions.map(d => d -> item % d)),
        op,
        i,
        trueTarget.toInt,
        falseTarget.toInt,
        0L,
        div.toInt
      )
  }.toVector

  val after10k = (0 until 10000).foldLeft(monkeys2) { (monkeys, round) =>
    val newMonkeys = monkeys.indices.foldLeft(monkeys) { (monkeys, index) =>
      val monkey = monkeys(index)
      val newMonkeys = monkey.itemRemainders.foldLeft(monkeys) { (monkeys, itemRems) =>
        val newItemRems = itemRems.map( (x, rem) => x -> monkey.applyOp(rem, x))
        val target = if (monkey.test(newItemRems)) monkey.trueTarget else monkey.falseTarget
        monkeys.updated(target, monkeys(target).copy(itemRemainders = monkeys(target).itemRemainders :+ newItemRems))
      }
      newMonkeys.updated(index, monkey.copy(itemRemainders = Vector.empty, business = monkey.business + monkey.itemRemainders.length))
    }
    if (round == 0 || round == 19 || (round + 1) % 1000 == 0) then
      newMonkeys.foreach(monkey => println(monkey.business))
      println()
    newMonkeys
  }

  val solution2 = after10k.map(_.business).sorted.takeRight(2).product
  println(solution2)
