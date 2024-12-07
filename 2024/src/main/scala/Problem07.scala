import scala.io.Source

object Problem07 extends App:
//  val file = "07-test.txt"
  val file = "07-input.txt"
  val input = Source.fromResource(file).getLines().toVector
  case class Equation(value: Long, components: Vector[Long]) {
    def validOperators(operators: Vector[String]): Vector[Vector[String]] =
      operators.map(op => Vector.fill(components.size-1)(op)).reduce(_ ++ _)
      .combinations(components.size - 1)
      .flatMap(_.permutations)
      .toVector
      .filter{ operatorList =>
        val ops: Vector[String] = Vector("+") ++ operatorList
        val res: Long = ops.indices.foldLeft(0L) {
          case (acc, i) if ops(i) == "+" => acc + components(i)
          case (acc, i) if ops(i) == "*" => acc * components(i)
          case (acc, i) if ops(i) == "||" => Math.pow(10, components(i).toString.length).toLong * acc + components(i)
        }
        res == value
      }
  }
  val equations = input.map {
    case s"$value: $components" => Equation(value.toLong, components.split(" ").toVector.map(_.toLong))
  }

  val result1 = equations.filter(_.validOperators(Vector("+", "*")).nonEmpty).map(_.value).sum
  println(result1)

  val result2 = equations.filter(_.validOperators(Vector("+", "*", "||")).nonEmpty).map(_.value).sum
  println(result2)
