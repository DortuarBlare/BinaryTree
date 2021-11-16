import DataStructure.BinaryTree
import Traits.Action
import TypeBuilders.IntegerBuilder

import java.util
import java.util.Vector
import scala.io.StdIn

object Main extends App {
  def timeTesting(binaryTree: BinaryTree[Int], size: Int) : Unit = {
    println("===============================================================")

    var lastTime = System.currentTimeMillis
    while (binaryTree.size != size) {
      binaryTree.add(typeBuilder.create())
    }
    println(s"Затрачено секунд на добавление ${binaryTree.size} узлов: " +
      s"${(System.currentTimeMillis - lastTime).toDouble / 1000}")

    println(s"Глубина левого поддерева: ${binaryTree.leftSubtreeDepth} | " +
      s"Глубина правого поддерева: ${binaryTree.rightSubtreeDepth}")

    lastTime = System.currentTimeMillis
    binaryTree.balance()
    println(s"\nЗатрачено секунд на балансировку: ${(System.currentTimeMillis - lastTime).toDouble / 1000}")

    println(s"Глубина левого поддерева: ${binaryTree.leftSubtreeDepth} | " +
      s"Глубина правого поддерева: ${binaryTree.rightSubtreeDepth}")


    lastTime = System.currentTimeMillis
    binaryTree.forEach(new Action[binaryTree.Node] {
      override def doWith(someObject: binaryTree.Node): Unit = {
        someObject.getValue
      }
    })
    println(s"\nЗатрачено секунд на forEach: ${(System.currentTimeMillis - lastTime).toDouble / 1000}")

    lastTime = System.currentTimeMillis
    for (i <- 0 until binaryTree.size) {
      binaryTree.findByIndex(i)
    }
    println(s"\nЗатрачено секунд на нахождение всех узлов: ${(System.currentTimeMillis - lastTime).toDouble / 1000}")

    while (binaryTree.size != 0) {
      binaryTree.deleteByIndex(0)
    }
    println(s"\nЗатрачено секунд на удаление всех узлов: ${(System.currentTimeMillis - lastTime).toDouble / 1000}")

    println("===============================================================\n")
  }

  def printTree(binaryTree: BinaryTree[Int]) : Unit = {
    binaryTree.forEach(println)
  }

  var typeBuilder = new IntegerBuilder()
  var binaryTree = new BinaryTree[Int](typeBuilder.getComparator())

  timeTesting(binaryTree, 1000)
  timeTesting(binaryTree, 5000)
  timeTesting(binaryTree, 10000)
  timeTesting(binaryTree, 25000)
  timeTesting(binaryTree, 50000)
}