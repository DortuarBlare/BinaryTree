package Traits

import java.io.InputStream

trait TypeBuilder {
  def typeName(): String
  def create(): Any
  def getComparator(): Traits.Comparator
}
