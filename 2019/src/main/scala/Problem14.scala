import scala.collection.{immutable, mutable}
import scala.io.Source

object Problem14 extends App {

  case class Dependency(target: String,  amount: Int, sources: Map[String, Int])

  val dependencies = Source.fromResource("14-input.txt").getLines().map { line =>
    val arr = line.split(" => ")
    val targetPart = arr(1).split(" ")
    val target = targetPart(1)
    val amount = targetPart(0).toInt
    val sources: Map[String, Int] = arr(0).split(", ").map{ source =>
      val sourcePart = source.split(" ")
      sourcePart(1) -> sourcePart(0).toInt
    }.toMap
    target -> Dependency(target, amount, sources)
  }.toMap

  val demands: mutable.Map[String, Int] = mutable.Map.empty

  demands.put("FUEL", 1)

  while (demands.view.filterKeys(_ != "ORE").values.exists(_ > 0)) {
    val (target, amount) = demands.view.filterKeys(_ != "ORE").find(_._2 > 0).get
    val dependency = dependencies(target)
    val factor = Math.ceil(amount.toDouble / dependency.amount.toDouble).toInt
    dependency.sources.foreach(source =>
      demands.put(source._1, demands.getOrElse(source._1, 0) + source._2*factor)
    )
    demands.put(target, amount - factor * dependency.amount)
  }

  println(demands("ORE"))

  val stock: mutable.Map[String, Long] = mutable.Map.empty
  stock.put("ORE", 1000000000000L)

  var done = false
  def produce(target: String, amount: Long): Boolean = {
    val dependency = dependencies(target)
    val factor = if (amount % dependency.amount == 0) amount / dependency.amount else amount / dependency.amount + 1
    val amountToProduce = dependency.amount * factor
//    println(s"Producing $amountToProduce $target because $amount needed")
    val results = dependency.sources.map { source =>
      val inStock = stock.getOrElse(source._1, 0L)
      val needed = (amountToProduce * source._2) / dependency.amount
//      if (source._1 == "ORE") println(s"Needs $needed ORE")
      val produced = if (inStock > needed) {
//        println(s"$needed ${source._1} already in stock")
        true
      } else if (source._1 != "ORE") {
        produce(source._1, needed - inStock)
      } else {
        false
      }
      if (produced)
        stock.put(source._1, stock(source._1) - ((source._2 * amountToProduce) / dependency.amount))
      produced
    }.toList
    if (results.forall(identity)) {
      stock.put(target, stock.getOrElse(target, 0L) + amountToProduce)
      true
    } else {
      false
    }
  }

  while (!done) {
    val amount = Math.max(stock("ORE") / demands("ORE").toLong, 1L)
    val oreBefore = stock("ORE")
    done = !produce("FUEL", amount)
    val oreAfter = stock("ORE")
    println(s"Produced $amount FUEL with result ${!done}, took ${oreBefore - oreAfter} ORE")
  }
  println(stock("FUEL"))

}
