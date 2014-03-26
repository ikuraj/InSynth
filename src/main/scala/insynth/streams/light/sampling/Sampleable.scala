package insynth.streams
package light
package sampling

import scala.util.Random

import scala.reflect._

trait Samplable[@specialized +T] {
  
  def sample(r: Random): T
  
}

//trait SamplableFun[@specialized +T] {
//  
//  def sample(sampler: Int => Int): T
//
//}

object Samplable {
  
  def apply[T](elems: T*)(implicit ct: ClassTag[T]): Enum[T] with Samplable[T] =
    elems match {
      case (stream: Stream[T]) if !stream.hasDefiniteSize =>
        throw new RuntimeException("Cannot sample a stream without knowing its definite size.")
      case _ if elems.size == 0 => Empty
      case _ if elems.size == 1 => new light.Singleton(elems.head) with Singleton[T]
      case _ => new WrapperArray(elems.toArray) with SamplableEnum[T]
    }
  
}