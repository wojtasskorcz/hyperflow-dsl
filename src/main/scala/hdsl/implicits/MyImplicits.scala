package hdsl.implicits

import hdsl.MutableMap

object MyImplicits {

  implicit class UniqueMutableMap[K, V](map: MutableMap[K, V]) {

    def putUnique(keyValue: (K, V)): Unit = keyValue match {
      case (key, value) => map.contains(key) match {
        case false => map += keyValue
        case true => throw new RuntimeException(s"Declaration of $key is not unique")
      }
    }

  }

}
