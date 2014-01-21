package insynth.streams.ordered

import insynth.streams._
import insynth.streams.unordered.{ Singleton => UnordSingleton }

case class Singleton[T](element: T, value: Int = 1) extends IntegerWeightStreamable[T] {
  
  override def size = 1
  
  override def toString = "ord.ST[ " + element.toString + ']'
  
  override def getValuedStream = Stream( (element, 1) )
  
}

object Singleton {

//  def apply[T](element: T) = new Singleton(element)

}