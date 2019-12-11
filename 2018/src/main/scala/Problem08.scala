import scala.io .Source

object Problem08 extends App {

  val input: List[Int] = Source.fromResource("08-input.txt").getLines().map(_.split(" ").map(_.toInt)).next().toList

  case class Node(children: List[Node], metadata: List[Int])

  def readNode(numbers: List[Int]): (Node, List[Int]) = {
    val children = numbers.head
    val metadataEntries = numbers.drop(1).head
    val (nodeWithChildren, numbersLeft) = (0 until children).foldLeft[(Node, List[Int])]((Node(Nil, Nil), numbers.drop(2))){ (acc, _) =>
      val (node, numbersLeft) = readNode(acc._2)
      acc._1.copy(children = acc._1.children ++ List(node)) -> numbersLeft
    }
    nodeWithChildren.copy(metadata = numbersLeft.take(metadataEntries)) -> numbersLeft.drop(metadataEntries)
  }

  val (node, _) = readNode(input)

  def metadataChecksum(node: Node): Int = {
    node.metadata.sum + node.children.map(metadataChecksum).sum
  }

  println(metadataChecksum(node))

  def nodeValue(node: Node): Int = node match {
    case Node(Nil, metadata) => metadata.sum
    case Node(children, metadata) => metadata.map(_ - 1).filter(index => index >= 0 && index < children.length).map(index => nodeValue(children(index))).sum
  }

  println(nodeValue(node))



}
