object Problem04 extends App {
  val solution = (264793 to 803935).map(_.toString).filter(number => number.sliding(2).exists(s => s(0) == s(1)) && number.sliding(2).forall(s => s(0).toInt <= s(1).toInt))
  println(solution.size)
  def strContainsMaxTwo(string: String): Boolean = {
    (string(0) == string(1) && string(2) != string(1)) || (string(3) != string(4) && string(4) == string(5)) || string.sliding(4).exists(str => str(1) == str(2) && str(0) != str(1) && str(3) != str(1))
  }
  val solution2 = solution.filter(strContainsMaxTwo)
  println(solution2.size)
}
