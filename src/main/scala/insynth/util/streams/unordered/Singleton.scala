package insynth.util.streams.unordered

import insynth.util.streams.Streamable

/**
 * stream of single element
 * @param <T>
 * @param element
 */
class Singleton[T](element: T) extends Streamable[T] {
  def isInfinite = false
  override def getStream = Stream(element)
}

object Singleton {
  def apply[T](element: T) = {
    new Singleton(element)
  }
}