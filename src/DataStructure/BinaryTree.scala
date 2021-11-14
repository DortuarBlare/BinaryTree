package DataStructure

import scala.util.control.Breaks.*

class BinaryTree[Type](private var comparator: Traits.Comparator) {
  class Node(private var value: Type) {
    private var weight = 1 // Вес узла (По умолчанию у всех равен 1)
    private var parent : Node = null
    private var leftChild : Node = null
    private var rightChild : Node = null

    def getValue = this.value // Геттер
    def setValue (newValue: Type) : Unit = { // Сеттер
      this.value = newValue
    }

    def getWeight = this.weight
    def setWeight (newWeight: Int) : Unit = {
      this.weight = newWeight
    }

    def getParent = this.parent
    def setParent (newParent: Node) : Unit = {
      this.parent = newParent
    }

    def getLeftChild = this.leftChild
    def setLeftChild (newLeftChild: Node) : Unit = {
      this.leftChild = newLeftChild
    }

    def getRightChild = this.rightChild
    def setRightChild (newRightChild: Node) : Unit = {
      this.rightChild = newRightChild
    }

    override def toString : String = {
      (if (parent == null) "ROOT " else "") +
        s"{ Value = $value, " +
        s"Weight = $weight, " +
        s"Parent = ${if (parent == null) "null" else parent.getValue}, " +
        s"LeftChild = ${if (leftChild == null) "null" else leftChild.getValue}, " +
        s"RightChild = ${if (rightChild == null) "null" else rightChild.getValue} }"
    }
  }

  private var root: Node = null

  def add (value: Type): Unit = {
    if (root == null) root = new Node(value)
    else if (findByValue(value) == null) {
      root.setWeight(root.getWeight + 1)
      var currentNode = root

      while (true) {
        if (comparator.compare(value, currentNode.getValue) < 0) { // Двигаемся влево
          if (currentNode.getLeftChild != null) {
            currentNode = currentNode.getLeftChild
            currentNode.setWeight(currentNode.getWeight + 1)
          }
          else {
            currentNode.setLeftChild(new Node(value))
            currentNode.getLeftChild.setParent(currentNode)
            return
          }
        }
        else { // Двигаемся вправо
          if (currentNode.getRightChild != null) {
            currentNode = currentNode.getRightChild
            currentNode.setWeight(currentNode.getWeight + 1)
          }
          else {
            currentNode.setRightChild(new Node(value))
            currentNode.getRightChild.setParent(currentNode)
            return None
          }
        }
      }
    }
  }

  def balance(): Unit = {
    var node : Node = null
    var leftSubtreeDepth = getDepth(root.getLeftChild)
    var rightSubtreeDepth = getDepth(root.getRightChild)

    while (Math.abs(leftSubtreeDepth - rightSubtreeDepth) > 1) {
      for (i <- 0 until size) {
        node = findByIndex(i)

        if (node == null) return None

        var child: Node = null
        var parent: Node = null
        var leftDepth = 0
        var rightDepth = 0
        breakable(while (true) {
          leftDepth = getDepth(node.getLeftChild)
          rightDepth = getDepth(node.getRightChild)

          if (leftDepth > rightDepth && leftDepth - rightDepth > 1) {
            // Правый поворот, так как глубина левого поддерева больше
            child = node.getLeftChild
            parent = node.getParent

            if (parent != null) {
              if (parent.getRightChild eq node) parent.setRightChild(child)
              else if (parent.getLeftChild eq node) parent.setLeftChild(child)
            }
            else root = child

            child.setParent(parent)
            node.setParent(child)

            node.setLeftChild(child.getRightChild)
            if (node.getLeftChild != null) {
              node.getLeftChild.setParent(node)
            }

            child.setRightChild(node)

            node.setWeight(1 + (if (node.getLeftChild != null) node.getLeftChild.getWeight else 0) +
              (if (node.getRightChild != null) node.getRightChild.getWeight else 0))
            child.setWeight(1 + (if (child.getLeftChild != null) child.getLeftChild.getWeight else 0) +
              (if (child.getRightChild != null) child.getRightChild.getWeight else 0))

            break

          }
          else if (rightDepth > leftDepth && rightDepth - leftDepth > 1) {
            // Левый поворот, так как глубина правого поддерева больше
            child = node.getRightChild
            parent = node.getParent

            if (parent != null) {
              if (parent.getRightChild eq node) parent.setRightChild(child)
              else if (parent.getLeftChild eq node) parent.setLeftChild(child)
            }
            else root = child

            child.setParent(parent)
            node.setParent(child)

            node.setRightChild(child.getLeftChild)
            if (node.getRightChild != null) {
              node.getRightChild.setParent(node)
            }

            child.setLeftChild(node)
            node.setWeight(1 + (if (node.getLeftChild != null) node.getLeftChild.getWeight else 0) +
              (if (node.getRightChild != null) node.getRightChild.getWeight else 0))
            child.setWeight(1 + (if (child.getLeftChild != null) child.getLeftChild.getWeight else 0) +
              (if (child.getRightChild != null) child.getRightChild.getWeight else 0))

            break

          }
          if (node.getParent != null) node = node.getParent
          else break
        })

      }
      leftSubtreeDepth = getDepth(root.getLeftChild)
      rightSubtreeDepth = getDepth(root.getRightChild)
    }
  }

  def findByValue(value: Type): Node = {
    var currentNode = root
    while (comparator.compare(value, currentNode.getValue) != 0) {
      if (comparator.compare(value, currentNode.getValue) < 0) currentNode = currentNode.getLeftChild
      else currentNode = currentNode.getRightChild

      if (currentNode == null) return null
    }
    currentNode
  }

  def findByIndex(index: Int): Node = {
    var currentNode = root
    var currentIndex = if (currentNode.getLeftChild != null) currentNode.getLeftChild.getWeight else 0
    while (index != currentIndex) {
      if (index < currentIndex) {
        currentNode = currentNode.getLeftChild
        if (currentNode == null) return null
        currentIndex -= (if (currentNode.getRightChild != null) currentNode.getRightChild.getWeight else 0) + 1
      }
      else {
        currentNode = currentNode.getRightChild
        if (currentNode == null) return null
        currentIndex += (if (currentNode.getLeftChild != null) currentNode.getLeftChild.getWeight else 0) + 1
      }
    }
    currentNode
  }

  def deleteByIndex(index: Int): Unit = {
    root.setWeight(root.getWeight - 1)
    var currentNode = root
    var currentIndex = if (currentNode.getLeftChild != null) currentNode.getLeftChild.getWeight else 0
    var isLeftChild = true

    while (index != currentIndex) { // Поиск удаляемого узла с заданным индексом
      if (index < currentIndex) {
        isLeftChild = true
        currentNode = currentNode.getLeftChild
        if (currentNode == null) return None
          currentIndex -= (if (currentNode.getRightChild != null) currentNode.getRightChild.getWeight else 0) + 1
      }
      else {
        isLeftChild = false
        currentNode = currentNode.getRightChild
        if (currentNode == null) return None
          currentIndex += (if (currentNode.getLeftChild != null) currentNode.getLeftChild.getWeight else 0) + 1
      }
      currentNode.setWeight(currentNode.getWeight - 1)
    }

    if (currentNode.getLeftChild == null && currentNode.getRightChild == null) { // Если у узла нет потомков
      if (currentNode eq root) root = null
      else if (isLeftChild) currentNode.getParent.setLeftChild(null)
      else currentNode.getParent.setRightChild(null)
    }
    else if (currentNode.getRightChild == null) { // Если у узла нет правого потомка(замена левым поддеревом)
      if (currentNode eq root) root = currentNode.getLeftChild
      else if (isLeftChild) currentNode.getParent.setLeftChild(currentNode.getLeftChild)
      else currentNode.getParent.setRightChild(currentNode.getLeftChild)
      currentNode.getLeftChild.setParent(currentNode.getParent)
    }
    else if (currentNode.getLeftChild == null) { // Если у узла нет левого потомка(замена правым поддеревом)
      if (currentNode eq root) root = currentNode.getRightChild
      else if (isLeftChild) currentNode.getParent.setLeftChild(currentNode.getRightChild)
      else currentNode.getParent.setRightChild(currentNode.getRightChild)
      currentNode.getRightChild.setParent(currentNode.getParent)
    }
    else { // Если у узла два потомка
      val heir = findHeir(currentNode)
      System.out.println("Преемник удаляемого узла: " + heir)
      if (currentNode eq root) root = heir
      else if (isLeftChild) currentNode.getParent.setLeftChild(heir)
      else currentNode.getParent.setRightChild(heir)
    }
    currentNode.setLeftChild(null)
    currentNode.setRightChild(null)
  }

  def findHeir(nodeThatNeedHeir: Node): Node = {
    var heir = if (nodeThatNeedHeir.getRightChild != null) nodeThatNeedHeir.getRightChild else nodeThatNeedHeir
    while (heir.getLeftChild != null) {
      heir.setWeight(heir.getWeight - 1)
      heir = heir.getLeftChild
    }
    if (heir eq nodeThatNeedHeir.getRightChild)  // Если наследник правый потомок
      heir.setLeftChild(nodeThatNeedHeir.getLeftChild)
    else {
      heir.getParent.setLeftChild(heir.getRightChild)
      heir.setLeftChild(nodeThatNeedHeir.getLeftChild)
      heir.setRightChild(nodeThatNeedHeir.getRightChild)
      heir.getLeftChild.setParent(heir)
      heir.getRightChild.setParent(heir)
    }
    heir.setParent(nodeThatNeedHeir.getParent) // Меняем родительскую связь

    heir.setWeight(nodeThatNeedHeir.getWeight)
    heir
  }

  def forEach(action: Traits.Action[Node]) : Unit = {
    var nextNode: Node = null
    var currentNodeValue: Any = null
    var size = this.size
    var foundFirstNode = false

    for (i <- 0 until size) {
      if (!foundFirstNode) { // Находим узел с индексом 0
        foundFirstNode = true
        nextNode = findByIndex(0)

        action.doThis(nextNode)
      }
      else {
        currentNodeValue = nextNode.getValue
        // Если есть правый потомок, тогда либо он,
        // либо последний его левый потомок будет следующим узлом
        if (nextNode.getRightChild != null) {
          nextNode = nextNode.getRightChild
          while (nextNode.getLeftChild != null) {
            nextNode = nextNode.getLeftChild
          }
          if (comparator.compare(currentNodeValue, nextNode.getValue) < 0)
            action.doThis(nextNode)
        }
        else if (nextNode.getParent != null) { // Иначе поднимаемся по родителям пока не найдем узел с бОльшим значением
          breakable(while (comparator.compare(currentNodeValue, nextNode.getParent.getValue) > 0) {
            nextNode = nextNode.getParent
            if (nextNode.getParent == null) break
          })
          if (nextNode.getParent != null) {
            if (comparator.compare(currentNodeValue, nextNode.getParent.getValue) < 0) {
              nextNode = nextNode.getParent
              action.doThis(nextNode)
            }
          }
        }
      }
    }
  }

  def getDepth(nodeForDepth: Node): Int = {
    var resultDepth = 0
    if (nodeForDepth != null) {
      val leftDepth = getDepth(nodeForDepth.getLeftChild)
      val rightDepth = getDepth(nodeForDepth.getRightChild)
      resultDepth = Math.max(leftDepth, rightDepth) + 1
    }
    resultDepth
  }

  def leftSubtreeDepth = getDepth(root.getLeftChild)

  def rightSubtreeDepth = getDepth(root.getRightChild)

  def size = (if (root == null) 0 else root.getWeight)

  def getRoot = this.root

  def getComparator = this.comparator
}
