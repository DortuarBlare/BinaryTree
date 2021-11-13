class BinaryTree[Type](private var comparator: Traits.Comparator) {
  private var root: Node = null

  def add (value: Type) : Unit = {
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
            return
          }
        }
      }
    }
  }

  def findByValue(value: Type) : Node = {
    var currentNode = root
    while (comparator.compare(value, currentNode.getValue) != 0) {
      if (comparator.compare(value, currentNode.getValue) < 0) currentNode = currentNode.getLeftChild
      else currentNode = currentNode.getRightChild

      if (currentNode == null) return null
    }
    currentNode
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

  def size = (if (root == null) 0 else root.getWeight)

  def getRoot = this.root

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
}
