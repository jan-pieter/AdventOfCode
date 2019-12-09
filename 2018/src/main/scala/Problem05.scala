import scala.io.Source

object Problem05 extends App {

  val chain = Source.fromResource("05-input.txt").getLines().next()

  //val chain = "dabAcCaCBAcCcaDA"

  private def react: (String, Char) => String = {
    (state, char) =>
      if (char.toUpper == state.last.toUpper && char != state.last)
        state.dropRight(1)
      else
        state + char
  }

  val result = chain.foldLeft("!")(react).tail


  println(result.length)

  val shortest = ('a' to 'z').map(toRemove => chain.filterNot(_.toLower == toRemove).foldLeft("!")(react).tail.length).min

  println(shortest)

}
