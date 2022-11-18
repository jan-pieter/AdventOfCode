import scala.io.Source

object Problem22 extends App {
  case class Instruction(state: Boolean, xMin: Int, xMax: Int, yMin: Int, yMax: Int, zMin: Int, zMax: Int) {
    def isWithin(x: Int, y: Int, z: Int, offset: Int): Boolean = {
      xMin + offset <= x && xMax + offset >= x &&
        yMin + offset <= y && yMax + offset >= y &&
        zMin + offset <= z && zMax + offset >= z
    }
    val size:Long = (xMax-xMin)*(yMax-yMin)*(zMax-zMin)
    def overlap(other: Instruction, within: Option[Instruction]): Long = {
      def innerOverlap(min1: Int, min2: Int, max1: Int, max2: Int): Long =
        if (min1 <= min2 && max1 >= max2)
          max2 - min2
        else if (min2 <= min1 && max2 >= max1)
          max1 - min1
        else if (min2 > max1 || max2 < min1)
          0L
        else
          max1.min(max2) - min1.max(min2)

      def outerOverlap(min1: Int, min2: Int, min3: Int, max1: Int, max2: Int, max3: Int): Long =
        if (min1 <= min2 && max1 >= max2)
          innerOverlap(min2, min3, max2, max3)
        else if (min2 <= min1 && max2 >= max1)
          innerOverlap(min1, min3, max1, max3)
        else if (min2 > max1 || max2 < min1)
          0L
        else
          innerOverlap(min1.max(min2), min3, max1.min(max2), max3)
      val overlapOnX: Long = within match {
        case Some(outerBox) => outerOverlap(xMin, outerBox.xMin, other.xMin, xMax, outerBox.xMax, other.xMax)
        case None => innerOverlap(xMin, other.xMin, xMax, other.xMax)
      }
      val overlapOnY: Long = within match {
        case Some(outerBox) => outerOverlap(yMin, outerBox.yMin, other.yMin, yMax, outerBox.yMax, other.yMax)
        case None => innerOverlap(yMin, other.yMin, yMax, other.yMax)
      }
      val overlapOnZ: Long = within match {
        case Some(outerBox) => outerOverlap(zMin, outerBox.zMin, other.zMin, zMax, outerBox.zMax, other.zMax)
        case None => innerOverlap(zMin, other.zMin, zMax, other.zMax)
      }
      overlapOnX * overlapOnY * overlapOnZ
    }
    def overlappingOn(instructions: Vector[Instruction], within: Option[Instruction]): Long = {
      instructions.foldLeft((0L, Vector.empty[Instruction])) {
        case ((on, applied), instruction) if instruction.state =>
          (on + instruction.overlap(this, within) - instruction.overlappingOn(applied, Some(this)), applied :+ instruction)
        case ((on, applied), instruction) =>
          (on - instruction.overlappingOn(applied, Some(this)), applied :+ instruction)
      }._1
    }
  }
  def parseInstruction(line: String): Instruction = line match {
    case s"on x=$xMin..$xMax,y=$yMin..$yMax,z=$zMin..$zMax" => Instruction(true, xMin.toInt, xMax.toInt, yMin.toInt, yMax.toInt, zMin.toInt, zMax.toInt)
    case s"off x=$xMin..$xMax,y=$yMin..$yMax,z=$zMin..$zMax" => Instruction(false, xMin.toInt, xMax.toInt, yMin.toInt, yMax.toInt, zMin.toInt, zMax.toInt)
  }

  val input = Source.fromResource("22-input.txt").getLines().map(parseInstruction).toVector
  val cube = Array.fill(101, 101, 101)(false)
  val offset = 50

  for {
    z <- cube.indices
    y <- cube.head.indices
    x <- cube.head.head.indices
  } yield {
    val endState = input.foldLeft(false){
      case (state, instruction) => if (instruction.isWithin(x, y, z, offset)) instruction.state else state
    }
    cube(z)(y)(x) = endState
  }

  val answer = cube.map(_.map(_.count(identity)).sum).sum
  println(s"Answer1: $answer")

  val (answer2, _) = input.foldLeft((0L, Vector.empty[Instruction])) {
    case ((on, applied), instruction) if instruction.state =>
      (on + instruction.size - instruction.overlappingOn(applied, None), applied :+ instruction)
    case ((on, applied), instruction) =>
      (on - instruction.overlappingOn(applied, None), applied :+ instruction)
  }
  println(s"Answer2: $answer2")


}
