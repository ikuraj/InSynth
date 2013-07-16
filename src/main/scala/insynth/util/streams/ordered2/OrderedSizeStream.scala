package insynth.util.streams.ordered2

import insynth.util.streams.{ Valuable }

trait OrderedSizeStreamable[+T] extends Valuable[Int] {
  def depleted: Boolean
  def nextReady(i: Int): Boolean
  def getStream: Stream[T]
}