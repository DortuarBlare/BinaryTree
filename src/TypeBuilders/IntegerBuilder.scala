package TypeBuilders

import Traits.Comparator

import java.io.InputStream
import scala.util.Random

class IntegerBuilder extends Traits.TypeBuilder {
  override def typeName() = "Int"

  override def create(): Int = new Random().nextInt(10000000) + 1

  override def getComparator(): Traits.Comparator = (o1: Any, o2: Any) =>
        o1.asInstanceOf[Int] - o2.asInstanceOf[Int]
}
