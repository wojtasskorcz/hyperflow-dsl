package hdsl.implicits

object MyImplicits {

  implicit class UniqueMap[K, V](map: Map[K, V]) {

    def +!(keyValue: (K, V)): Map[K, V] = keyValue match {
      case (key, value) => map.contains(key) match {
        case false => map + keyValue
        case true => throw new RuntimeException(s"Declaration of $key is not unique")
      }
    }

  }

}
