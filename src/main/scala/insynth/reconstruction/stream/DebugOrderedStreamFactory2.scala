package insynth.reconstruction.stream

import insynth.util.streams.ordered2._
import insynth.util.streams.ordered2.{ OrderedSizeStreamable => Streamable }

import insynth.util.logging.HasLogger

class DebugOrderedStreamFactory2[T] extends HasLogger {
  
   def makeEmptyStreamable = Empty

   def makeSingleton[U <: T](element: U) = Singleton(element)
  
   def makeSingletonList[U <: T](element: List[U]) = Singleton(element)
  
//   def makeSingleStream[U <: T](stream: => Stream[U], isInfiniteFlag: Boolean) =
//    SingleStream(stream, isInfiniteFlag)
  
   def makeUnaryStream[X, Y <: T](streamable: Streamable[X], modify: X=>Y, name: String, modifyVal: Option[Int => Int] = None) =
    UnaryStream(streamable.asInstanceOf[OrderedSizeStreamable[X]], name, modify, modifyVal)
  
   def makeUnaryStreamList[X, Y <: T](streamable: Streamable[X], modify: X => List[Y]) =
    UnaryStream(streamable.asInstanceOf[OrderedSizeStreamable[X]], "param", modify)
  
   def makeBinaryStream[X, Y, Z <: T](s1: Streamable[X], s2: Streamable[Y], name: String)(combine: (X, Y) => List[Z]) =
    BinaryStream(s1.asInstanceOf[OrderedSizeStreamable[X]], s2.asInstanceOf[OrderedSizeStreamable[Y]], name: String)(combine)
     
   def makeRoundRobbin[U <: T](streams: Seq[Streamable[U]], name: String) = {
     RoundRobbin(streams.asInstanceOf[Seq[OrderedSizeStreamable[U]]],name)
   }
  
   def makeLazyRoundRobbin[U <: T](initStreams: List[Streamable[U]], name: String) =
     LazyRoundRobbin[U](initStreams.asInstanceOf[List[OrderedSizeStreamable[U]]], name: String)
      
  def getFinalStream(streamable: Streamable[T]) = 
    streamable match {
      case os: OrderedSizeStreamable[_] =>
        fine("returning ordered streamable")
        os.getStream zip os.getValues.map(_.toFloat)
//      case _: Streamable[_] =>
//        fine("returning unordered streamable")
//        streamable.getStream zip Stream.continually(0f)
    }
}