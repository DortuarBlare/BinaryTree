package TypeBuilders

import java.io.{BufferedReader, InputStream, InputStreamReader}
import java.util.Random
import java.util.stream.Collectors

class StringBuilder extends Traits.TypeBuilder {
  override def typeName() = "String"

  override def create(): String = getRandomString(10)

  def getRandomString(length: Int): String = {
    val allSymbols = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    val random = new Random
    val sb = new StringBuffer
    for (i <- 0 until length) {
      val number = random.nextInt(allSymbols.length - 1)
      sb.append(allSymbols.charAt(number))
    }
    sb.toString
  }

  override def getComparator(): Traits.Comparator = (o1: Any, o2: Any) =>
    o1.asInstanceOf[String].compareTo(o2.asInstanceOf[String])
}
