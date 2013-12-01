package insynth.streams.ordered

import insynth.streams._
import insynth.streams.unordered.{ SingleStream => UnSingleStream }

// NOTE this would require ordered stream
class FiniteStream[T](arr: => Vector[(T, Int)])
	extends OrderedStreamable[T] {
  
  var nextInd = 0
  val stream = Stream.fill( arr.size )( { nextInd+=1; arr(nextInd-1) } )
  
  override def isInfinite = false
  
  override def isDepleted: Boolean = nextInd < arr.size // wtv
  override def nextReady(ind: Int): Boolean = true
  
  override def getStream = stream map { _._1 }
  
  override def getValues = stream map { _._2 }
    
}

object FiniteStream {
  def apply[T](stream: => Vector[(T, Int)]) =
    new FiniteStream(stream)
}