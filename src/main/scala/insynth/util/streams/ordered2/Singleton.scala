package insynth.util.streams.ordered2

import insynth.util.streams.unordered.{ Singleton => UnordSingleton }

class Singleton[T](element: T) extends OrderedSizeStreamable[T] {
  
  override def depleted: Boolean = true // wtv
  override def nextReady(ind: Int): Boolean = ind == 0
    
  override def getStream = Stream(element)
  
  override def toString = element.toString
  
  override def getValues = Stream(1)
  
}

object Singleton {
  def apply[T](element: T) = new Singleton(element)
}