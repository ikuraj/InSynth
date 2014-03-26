package insynth.streams
package light
package sampling

import scala.util.Random

trait Singleton[T] extends light.Singleton[T] with Samplable[T] {
  
  def sample(sampler: Random) =
    this.el
  
}