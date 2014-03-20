package insynth.streams
package light
package weight

import scala.reflect._
import scala.language.implicitConversions

trait WeightEnum[+A, @specialized(Int, Float) V] extends Enum[(A, V)] with Weighted[V] {
    
  def getWeight(ind: Int) = this(ind)._2
  
}