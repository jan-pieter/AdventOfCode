import scala.io.Source
import scala.collection.mutable
import Problem20.Pulse.*

object Problem20 extends App:
  enum Pulse:
    case Low, High

  sealed trait Module:
    def name: String
    def destinations: Vector[String]
    def receive(from: String, pulse: Pulse): (Module, Vector[(String, String, Pulse)])

  case class Broadcaster(destinations: Vector[String]) extends Module:
    val name: String = "broadcaster"
    def receive(from: String, pulse: Pulse): (Module, Vector[(String, String, Pulse)]) =
      (this, destinations.map(d => (name, d, pulse)))

  case class FlipFlop(name: String, destinations: Vector[String], state: Boolean = false) extends Module:
    def receive(from: String, pulse: Pulse): (Module, Vector[(String, String, Pulse)]) = pulse match {
      case High => (this, Vector.empty)
      case Low => (this.copy(state = !state), destinations.map(d => (name, d, if state then Low else High)))
    }

  case class Conjunction(name: String, destinations: Vector[String], memory: Map[String, Pulse] = Map.empty) extends Module:
    def receive(from: String, pulse: Pulse): (Module, Vector[(String, String, Pulse)]) =
      val newMemory = memory.updated(from, pulse)
      (this.copy(memory = newMemory), destinations.map(d => (name, d, if newMemory.forall(_._2 == High) then Low else High)))

  object Module:
    def fromString(s: String): Module = s match {
      case s"%$name -> $destinations" => FlipFlop(name, destinations.split(", ").toVector)
      case s"&$name -> $destinations" => Conjunction(name, destinations.split(", ").toVector)
      case s"broadcaster -> $destinations" => Broadcaster(destinations.split(", ").toVector)
    }

  val input = Source.fromResource("20-input.txt").getLines().toVector.map(Module.fromString)
  val updatedConjunctions = input.map {
    case m: Conjunction => m.copy(memory = input.filter(_.destinations.contains(m.name)).map(_.name -> Low).toMap)
    case m => m
  }

  val moduleMap: mutable.Map[String, Module] = mutable.Map(updatedConjunctions.map(m => m.name -> m): _*)
  var lowPulses: Long = 0L
  var highPulses: Long = 0L
  var buttonPushes: Long = 0L
  def pushButton() = {
    buttonPushes = buttonPushes+1
    val queue: mutable.Queue[(String, String, Pulse)] = mutable.Queue(("button", "broadcaster", Low))
    while (queue.nonEmpty) {
      val (source, destination, pulse) = queue.dequeue()
      if pulse == Low then lowPulses = lowPulses + 1 else highPulses = highPulses + 1
      if (destination == "ll" && pulse == High) then println(s"Push $buttonPushes: $source => $destination: $pulse")
      moduleMap.get(destination).map { module =>
        val (newModule, pulses) = module.receive(source, pulse)
        moduleMap.update(destination, newModule)
        queue.enqueueAll(pulses)
      }
    }
  }
  (0 until 1000).foreach(_ => pushButton())
  println(lowPulses*highPulses)

  (0 until 9000).foreach(_ => pushButton())

  def lcm(list: Seq[Long]): Long = list.foldLeft(1L) {
    (a, b) => b * a / LazyList.iterate((a, b)) { case (x, y) => (y, x % y) }.dropWhile(_._2 != 0).head._1.abs
  }

  println(lcm(Vector(3793L, 3917L, 4013L, 4051L)))
