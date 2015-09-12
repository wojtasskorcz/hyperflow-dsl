package hdsl.parser.structures.traits

trait Instantiated extends PropertyContainer {

  def name: String
  def instantiation: Instantiation
  def putInstanceOnlyToVisible(visibleName: String): Unit
  def putInstanceToVisibleAndAll(visibleName: String): Unit

}
