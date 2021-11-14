package Traits

trait Action[Type] {
  def doThis(someObject: Type): Unit
}
