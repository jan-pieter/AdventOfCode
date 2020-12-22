import scala.io.Source

object Problem21 extends App {
  val input = Source.fromResource("21-input.txt").getLines().toList

  case class Food(ingredients: Set[String], allergens: Set[String])

  val foods: List[Food] = input.map { line =>
    val parts = line.split('(')
    Food(parts(0).dropRight(1).split(" ").toSet, parts(1).drop("contains ".length).dropRight(1).split(", ").toSet)
  }

  val ingredients: Set[String] = foods.flatMap(_.ingredients).toSet
  val allergens: Set[String] = foods.flatMap(_.allergens).toSet

  val allergenMap: Map[String, Set[String]] = allergens.map{ allergen =>
    val foodsWithAllergen: List[Food] = foods.filter(_.allergens.contains(allergen))
    val candidates: Set[String] = foodsWithAllergen.foldLeft(ingredients){
      case (candidates, food) => candidates.intersect(food.ingredients)
    }
    allergen -> candidates
  }.toMap
  val allCandidates: Set[String] = allergenMap.values.flatten.toSet

  val clearIngredients: Set[String] = ingredients.diff(allCandidates)

  println(clearIngredients.toList.map(ingredient => foods.count(_.ingredients.contains(ingredient))).sum)

  println(allergenMap)

  // dairy -> zfcqk
  // fish -> mdtvbb
  // nuts -> ggdbl
  // peanuts -> frpvd
  // sesame -> mgczn
  // shellfish -> zsfzq
  // soy -> kdqls
  // wheat -> kktsjbh

}
