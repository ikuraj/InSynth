package insynth
package streams
package ordered

import unordered.{ SingleStream => UnSingleStream }
import util.logging._

class FiniteStream[T](coll: => Seq[(T, Int)])
	extends IntegerWeightStreamable[T] with HasLogger {
  require(coll.hasDefiniteSize)
  require(coll.sortBy(_._2) == coll, "Given collection must be sorted")
  
  override def isInfinite = false
  
  override def getValuedStream = coll.toStream
  
  override def size = coll.size
    
}

object FiniteStream {
  def apply[T](stream: => Seq[(T, Int)]) =
    new FiniteStream(stream)
}