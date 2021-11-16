package Traits

trait Action[Type] {
  def doWith(someObject: Type): Unit
}
