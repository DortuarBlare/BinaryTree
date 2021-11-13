import java.util
import java.util.Vector
import scala.io.StdIn

object Main extends App {
  var typeBuilder = new IntegerBuilder()
  var binaryTree = new BinaryTree[Int](typeBuilder.getComparator())
  //val values = Vector()
  val values = new util.Vector[Int]
  while (binaryTree.size != 10) {
    var value = typeBuilder.create()
    binaryTree.add(value)
    values.add(value)
  }
  var index = 0
  while (index != 10) {
    println(binaryTree.findByValue(values.elementAt(index)))
    index += 1
  }
}