package insynth.streams
package light
package weight

import scala.reflect._
import scala.collection.mutable

import insynth.util.logging._

class RoundRobbinEager[T] protected[streams]
  (finite: FiniteIntegerWeightEnum[T], infinite: FiniteIntegerWeightEnum[T])
  extends IntegerWeightEnum[T] with Finite[(T, Int)] with HasLogger {
  
  override def apply(ind: Int) = {
    if (ind < finite.size) finite(ind)
    else
      infinite(ind - finite.size)
  }
    
}