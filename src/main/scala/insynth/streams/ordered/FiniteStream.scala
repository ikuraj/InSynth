package insynth.streams.ordered

import insynth.streams._
import insynth.streams.unordered.{ SingleStream => UnSingleStream }

class FiniteStream[T](elements: => Seq[(T, Int)])
	extends OrderedStreamable[T] {

  private val buffer = elements.toArray
  private var currInd: Int = 0
  
  override def isInfinite = false
  
  override def isDepleted: Boolean = currInd == buffer.size - 1
  override def nextReady(ind: Int): Boolean = ind < buffer.size - 1
  
  override def getStream = buffer.toStream.map(_._1)
  
  override def getValues = buffer.toStream.map(_._2)
    
}
