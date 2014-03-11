package insynth.streams.light

object Empty extends Enumerable[Nothing] {
    
  override def size = 0

  override def apply(ind: Int) = throw new NoSuchElementException("no elements in Empty")

}