package insynth.util.streams.ordered

import insynth.util.streams.{ Streamable, Valuable }

trait OrderedSizeStreamable[T] extends Streamable[T] with Valuable[Int]