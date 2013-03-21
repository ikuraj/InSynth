package insynth.util.streams

trait Valuable[T] {
  def getValues: Stream[T]
}