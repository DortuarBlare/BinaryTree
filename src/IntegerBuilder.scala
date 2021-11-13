import Traits.Comparator

import java.io.InputStream
import scala.util.Random

class IntegerBuilder extends Traits.TypeBuilder {
  override def typeName() : String = {
    "Integer"
  }

  override def create() : Int = {
    new Random().nextInt(10000000) + 1
  }

  override def getComparator() : Traits.Comparator = {
    new Comparator {
      override def compare(object1: Any, object2: Any) : Int =
        object1.asInstanceOf[Int] - object2.asInstanceOf[Int]
    }
  }
}
