package insynth.streams.light
package sampling

import scala.util.Random

trait RandomSampler[@specialized T] extends Samplable[T] with SamplableFun[T] {
  
  def size: Int
  
  def sample(r: Random) =
    sample( () => r.nextInt( size ) )
  
  def sample(sampler: Int => Int): T 
    

}