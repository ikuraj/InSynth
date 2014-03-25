package insynth.streams.light
package sampling

import scala.reflect._
import scala.language.implicitConversions

trait SamplableEnum[T] extends Samplable[T] with SamplableFun[T] {
  
  this: RandomSampler[T] =>
      
  def sample(sampler: () => Int) =
    this.apply( sampler() )
  
}