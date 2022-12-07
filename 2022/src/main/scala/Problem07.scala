import scala.io.Source

object Problem07 extends App:
  val input = Source.fromResource("07-input.txt").getLines().toVector

  case class State(currentDir: Vector[String], dirs: Vector[String], files: Vector[(String, Long)]) {
    def dirSize(name: String): Long = files.filter(_._1.startsWith(name)).map(_._2).sum
  }

  val state = input.foldLeft(State(Vector(""), Vector.empty, Vector.empty)) {
    case (state, "$ cd /") => state.copy(Vector(""), state.dirs :+ "/")
    case (state, "$ cd ..") => state.copy(currentDir = state.currentDir.dropRight(1))
    case (state, s"$$ cd $name") => state.copy(currentDir = state.currentDir :+ name)
    case (state, "$ ls") => state
    case (state, s"dir $name") => state.copy(dirs = state.dirs :+ (state.currentDir :+ name).mkString("/"))
    case (state, s"$size $name") => state.copy(files = state.files :+ ((state.currentDir :+ name).mkString("/") -> size.toLong))
  }

  val dirsWithSizes = state.dirs.map(name => name -> state.dirSize(name)).toMap

  val solution1 = dirsWithSizes.toVector.filter((_, size) => size <= 100000L).map(_._2).sum
  println(solution1)

  val freeSpace = 70000000L - dirsWithSizes("/")
  val toFreeUp  = 30000000L - freeSpace
  val solution2 = dirsWithSizes.filter((_, size) => size >= toFreeUp).toVector.sortBy(_._2).head._2
  println(solution2)
