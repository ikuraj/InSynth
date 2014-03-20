package insynth.streams
package light
package weight

import scala.reflect._
import insynth.util.logging.HasLogger

class WrapperArray[@specialized T](coll: Array[(T, Int)])
	extends IntegerWeightEnum[T] with Finite[(T, Int)] with HasLogger {
  require(coll.hasDefiniteSize)
  
  override def size = coll.size
  
  override def apply(ind: Int) = coll(ind)
    
}