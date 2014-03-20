package insynth.streams
package light
package weight

import insynth.streams._
import insynth.streams.unordered.{ SingleStream => UnSingleStream }

/**
 * Wrapper around the Scala stream
 * NOTE: parameter stream needs to be ordered itself
 */
class WrapperStream[T](stream: Stream[(T, Int)])
	extends IntegerWeightEnum[T] with Infinite[(T, Int)] {
  
  override def apply(ind: Int) = stream(ind)
    
  override def size = -1
}