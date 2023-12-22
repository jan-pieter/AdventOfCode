import scala.collection.mutable
import scala.io.Source

object Problem22 extends App:
  case class Brick(id: Int, x1: Int, y1: Int, z1: Int, x2: Int, y2: Int, z2: Int)
  val input = Source.fromResource("22-input.txt").getLines().toVector.zipWithIndex.map {
    case (s"$x1,$y1,$z1~$x2,$y2,$z2", i) =>
      val xs = Vector(x1.toInt, x2.toInt).sorted
      val ys = Vector(y1.toInt, y2.toInt).sorted
      val zs = Vector(z1.toInt, z2.toInt).sorted
      Brick(i, xs(0), ys(0), zs(0), xs(1), ys(1), zs(1))
  }.sortBy(_.z1)
//  input.foreach(println(_))
  println(input.size)

  val maxZ = input.map(brick => brick.z2).max
  val maxY = input.map(brick => brick.y2).max
  val maxX = input.map(brick => brick.x2).max
  println(s"maxZ $maxZ maxY $maxY maxX $maxX")

  def settle(bricks: Vector[Brick]): Vector[Vector[Vector[Option[Brick]]]] =
    bricks.foldLeft(Vector.fill[Option[Brick]](maxZ + 1, maxY + 1, maxX + 1)(None)) { (soFar, brick) =>
      var fallen = 0
      while (brick.z1 - fallen - 1 > 0 && soFar(brick.z1 - fallen - 1).slice(brick.y1, brick.y2+1).forall(_.slice(brick.x1, brick.x2+1).forall(_.isEmpty))) {
        //      println(s"Increase fallen for brick $brick")
        fallen = fallen + 1
      }
      //    println(s"Not empty: ${soFar(brick.z1-fallen-1)}")
      val settledBrick = brick.copy(z1 = brick.z1 - fallen, z2 = brick.z2 - fallen)
      (for {
        z <- settledBrick.z1 to settledBrick.z2
        y <- settledBrick.y1 to settledBrick.y2
        x <- settledBrick.x1 to settledBrick.x2
      } yield (z, y, x)).foldLeft(soFar) {
        case (updated, (z, y, x)) =>
          updated.updated(z, updated(z).updated(y, updated(z)(y).updated(x, Some(settledBrick))))
      }
    }

  val settled = settle(input)

//  settled.flatten.flatten.flatten.toSet.foreach(println(_))
  val settledBricks = settled.flatten.flatten.flatten.toSet
  println(settledBricks.size)
  val answer1 = settledBricks.count { brick =>
    (for {
      y <- brick.y1 to brick.y2
      x <- brick.x1 to brick.x2
    } yield {
//      println(s"Above brick $brick at $y $x: ${settled(brick.z2 + 1)(y)(x)}")
      settled(brick.z2 + 1)(y)(x)
    }).flatten.forall{ above =>
      !(for {
        y <- above.y1 to above.y2
        x <- above.x1 to above.x2
      } yield {
        settled(above.z1 - 1)(y)(x)
      }).flatten.forall(_ == brick)
    }
  }
  println(answer1)

  def fall(brick: Brick): Set[Brick] =
    var fallen = Set.empty[Brick]
    given Ordering[Brick] = Ordering.by[Brick, Int](_.z1).reverse
    val queue: mutable.PriorityQueue[Brick] = mutable.PriorityQueue(brick)
    while (queue.nonEmpty) {
      val nextBrick = queue.dequeue()
      fallen = fallen + nextBrick
      val wouldFall = (for {
        y <- nextBrick.y1 to nextBrick.y2
        x <- nextBrick.x1 to nextBrick.x2
      } yield {
        //      println(s"Above brick $brick at $y $x: ${settled(brick.z2 + 1)(y)(x)}")
        settled(nextBrick.z2 + 1)(y)(x)
      }).flatten.toSet.filter { above =>
        (for {
          y <- above.y1 to above.y2
          x <- above.x1 to above.x2
        } yield {
          settled(above.z1 - 1)(y)(x)
        }).flatten.forall(fallen)
      }
//      println(s"Would fall $wouldFall")
      queue.addAll(wouldFall)
    }
//    println(s"Fallen $fallen")
    fallen

  val answer2 = settledBricks.toVector.map{brick => fall(brick).size - 1}.sum
  println(answer2)

  // 90765 too low
  // 96356
