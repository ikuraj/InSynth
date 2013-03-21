package insynth.util.streams.unordered

import scala.util.Random

import org.scalatest.FunSuite

import insynth.util.streams.Streamable

class SingleStreamTest extends FunSuite {    
  
  def printStream[A](stream: Stream[A])(n: Int) = stream.take(n).toList mkString (", ")
  
  test("infinite stream test") {
    val randomInt = new Random(System.currentTimeMillis()).nextInt
  
    val infiniteStream = Stream.continually(randomInt)
    
    val streamable: Streamable[Int] = SingleStream(infiniteStream, true)
    
    assert(streamable.isInfinite)
    
    val stream = streamable.getStream
    
    expect(100) { stream.take(100).size }
    
  }
    
  test("finite stream test")  {
    
    val finiteStream = List(1,2,3).toStream
    
    val streamable: Streamable[Int] = SingleStream(finiteStream, false)
    
    assert(!streamable.isInfinite)
    
    val stream = streamable.getStream
    
    expect(6) { (0 /: stream)(_ + _) }
    
  }
  
}