import scala.io.Source

object Problem03 extends App {

  val input = Source.fromResource("03-input.txt").getLines().toVector

  val length = input.head.length

  val gammaStr = (0 until length).map{
    i =>
      val inputs = input.map(_(i).toString.toInt).sum
     //println(inputs)
      if (input.map(_(i).toString.toInt).sum > input.length / 2) "1" else "0"
  }.mkString

  val epsilonStr = gammaStr.map {
    case '0' => '1'
    case '1' => '0'
  }.mkString

  val gamma = Integer.parseInt(gammaStr, 2)
  val epsilon = Integer.parseInt(epsilonStr, 2)

  println(s"Gamma $gammaStr = $gamma Epsilon $epsilonStr = $epsilon => ${gamma * epsilon}")

  var oxygens = input
  var i = 0

  while(oxygens.size != 1) {
    val inputs = oxygens.map(_(i).toString.toInt).sum
    //println(inputs)

    val common = if (oxygens.map(_(i).toString.toInt).sum.toDouble >= oxygens.length / 2D) "1" else "0"
    oxygens = oxygens.filter(_(i).toString == common)
    //println(s"$common $oxygens")
    i = i+1
  }

  println("")

  var co2s = input
  var j = 0

  while(co2s.size != 1) {
    val common = if (co2s.map(_(j).toString.toInt).sum.toDouble >= co2s.length / 2D) "0" else "1"
    co2s = co2s.filter(_(j).toString == common)
    //println(s"$common $co2s")
    j = j+1
  }

  val oxygenStr = oxygens.head
  val co2Str = co2s.head
  val oxygen = Integer.parseInt(oxygenStr, 2)
  val co2 = Integer.parseInt(co2Str,2)

  println(s"Oxygen ${oxygenStr} = $oxygen CO2 ${co2Str} = $co2 => ${oxygen*co2} ")



}
