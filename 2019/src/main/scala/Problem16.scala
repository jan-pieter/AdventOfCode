import java.math.{MathContext, RoundingMode}

import scala.collection.immutable
import scala.io.Source

object Problem16 extends App {

//  val data = Vector(1,2,3,4,5,6,7,8)
  val data = Source.fromResource("16-test.txt").getLines().next().map(_.asDigit).toVector
  val multipliedData = Stream.continually(data).take(10000).flatten.toVector
  val offset = data(6) + data(5)*10 + data(4)*100 + data(3) * 1000 + data(2)*10000 + data(1)*100000 + data(0)*1000000
  println(s"Offset: $offset")

  val pattern = Vector(0, 1, 0, -1)

  def repeat(input: Vector[Int], repeats: Int): Vector[Int] =
    input.flatMap(x => Vector.fill(repeats)(x))

 // val factors: immutable.IndexedSeq[Stream[Int]] = multipliedData.indices.map(x => Stream.continually(repeat(pattern, x+1)).flatten.drop(1))

  def factor(index: Int, repeats: Int): Int = {
    pattern(Math.round((index+1).toDouble / repeats.toDouble).toInt % 4)
  }

  def factor2(index: Int, repeats: Int): Int = {
    val step = 90D / repeats.toDouble
    val result = (index + 1).toDouble * step % 360
    val sin = BigDecimal(Math.sin(Math.toRadians(result))).setScale(12, BigDecimal.RoundingMode.HALF_UP)
    val cos = BigDecimal(Math.cos(Math.toRadians(result))).setScale(12, BigDecimal.RoundingMode.HALF_UP)
//    println(s"Index $index repeats $repeats step $step result $result sin $sin cos $cos")
    if (sin < 0 && cos >= 0) {
      -1
    } else if (sin <= 0 && cos < 0) {
      0
    } else if (sin >= 0 && cos > 0) {
      0
    } else if (sin > 0 && cos <= 0) {
      1
    } else {
      System.exit(1)
      -10
    }
  }

//  for {
//    y <- 1 until 9
//    x <- 0 until 8
//  } yield {
//    println(s"factor2($x,$y): ${factor2(x,y)}")
//  }
//  System.exit(0)
//  println("Factors done")

//  println(s"Factors done: ${factors.map(_.take(8).mkString).mkString(" ")}")

  def fft(input: Vector[Int]): Vector[Int] = {
    input.indices.map(index =>
      Math.abs(
        input
          .zipWithIndex
          .map{tuple =>
//            println(s"${tuple._1} * ${factor2(tuple._2, index+1)} (factor(${tuple._2},${index+1})");
            tuple._1 * factor2(tuple._2, index+1)
          }
          .sum
          % 10
      )
    ).toVector
  }

  val result = (0 until 100).foldLeft(multipliedData){(input, _) =>
    val result = fft(input)
    println(result.take(8).mkString)
    result
  }

  println(result.slice(offset, offset + 8).mkString)

}

trait FFT {
  import scala.math._

  case class Complex(re: Double, im: Double = 0.0) {
    def +(x: Complex): Complex = Complex((this.re+x.re), (this.im+x.im))
    def -(x: Complex): Complex = Complex((this.re-x.re), (this.im-x.im))
    def *(x: Complex): Complex = Complex(this.re*x.re-this.im*x.im, this.re*x.im+this.im*x.re)
  }

  def transformReal(input:IndexedSeq[Double]) = {
    val data = padder(input.map(i => Complex(i)).toList)
    val outComplex = fft(data)
    outComplex.map(c => math.sqrt((c.re * c.re) + (c.im * c.im))).take((data.length / 2) + 1).toIndexedSeq // Magnitude Output
  }

  def powerSpectrum(input:IndexedSeq[Double]) = {
    val data = padder(input.map(i => Complex(i)).toList)
    val outComplex = fft(data)
    val out = outComplex.map(c => math.sqrt((c.re * c.re) + (c.im * c.im))).take((data.length / 2) + 1).toIndexedSeq
    out.map(i => (i * i) / data.length) // Power Spectral Density Output
  }

  def padder(data:List[Complex]) : List[Complex] = {
    def check(num:Int) : Boolean = if((num.&(num-1)) == 0) true else false
    def pad(i:Int) : Int = {
      check(i) match {
        case true => i
        case false => pad(i + 1)
      }
    }
    if(check(data.length) == true) data else data.padTo(pad(data.length), Complex(0))
  }

  def fft(f: List[Complex]) : List[Complex] = {
    f.size match {
      case 0 => Nil
      case 1 => f
      case n => {
        val c: Double => Complex = phi => Complex(cos(phi), sin(phi))
        val e = fft(f.zipWithIndex.filter(_._2%2==0).map(_._1))
        val o  = fft(f.zipWithIndex.filter(_._2%2!=0).map(_._1))
        def it(in:List[(Int, Complex)], k:Int = 0) : List[(Int, Complex)] = {
          k < (n / 2) match {
            case true => it( (k+n/2,e(k)-o(k)*c(-2*Pi*k/n)) :: (k,e(k)+o(k)*c(-2*Pi*k/n)) :: in, k + 1)
            case false => in
          }
        }
        it(List[(Int, Complex)]()).sortWith((x,y) => x._1 < y._1).map(_._2)
      }
    }
  }
}