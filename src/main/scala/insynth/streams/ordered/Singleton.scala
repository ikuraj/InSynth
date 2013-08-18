package insynth.streams.ordered

import insynth.streams.unordered.{ Singleton => UnordSingleton }

class Singleton[T](element: T) extends UnordSingleton(element) with OrderedSizeStreamable[T] {
  
  override def getValues = Stream(1)
  
}

object Singleton {
  def apply[T](element: T) = new Singleton(element)
}