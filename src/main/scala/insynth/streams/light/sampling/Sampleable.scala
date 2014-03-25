package insynth.streams.light
package sampling

import scala.util.Random

trait Samplable[@specialized T] extends Finite[T] {
  
  def sample(r: Random): T
  
}

trait SamplableFun[@specialized T] extends Finite[T] {
  
  def sample(sampler: Int => Int): T

}