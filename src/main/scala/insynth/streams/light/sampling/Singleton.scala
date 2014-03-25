package insynth.streams
package light
package sampling

trait Singleton[T] extends RandomSampler[T] {
  
  this: light.Singleton[T] =>
  
  def sample(sampler: () => Int) =
    this.el
  
}