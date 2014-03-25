package insynth.streams
package light
package sampling

import insynth.util.logging._

import dependent.BinaryFiniteCombineLazy

trait BinaryChain[I, O, R] extends RandomSampler[R] with HasLogger {
  
  this: BinaryFiniteCombineLazy[I, O, R] =>
    
  def sample(sampler: Int => Int) = {
    val firstLevelIndex = sampler( this.s1.size )
    val secondLevelIndex = sampler ( this.s2.size )
    
    val innerEnum = s2( s1(firstLevelIndex) )
  }
  
}