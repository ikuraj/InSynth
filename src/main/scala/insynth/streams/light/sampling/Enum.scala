package insynth.streams.light
package sampling

import scala.util.Random

import scala.reflect._
import scala.language.implicitConversions

trait SamplableEnum[T] extends Enum[T] with Samplable[T] {
        
  def sample(sampler: Random) =
    this.apply( sampler.nextInt(this.size) )
  
}