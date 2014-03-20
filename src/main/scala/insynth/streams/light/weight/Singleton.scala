package insynth.streams
package light
package weight

class Singleton[T](pair: (T, Int)) extends WeightEnum[T, Int] with Finite[(T, Int)] {
  
  override def size = 1
  
  override def apply(ind: Int) = 
    if (ind == 0) pair
    else throw new NoSuchElementException("Singleton has only one element")
  
}