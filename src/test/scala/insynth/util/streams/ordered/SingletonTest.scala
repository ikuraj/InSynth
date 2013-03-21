package insynth.util.streams.ordered

import scala.util.Random

import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen

class SingletonTest extends FunSpec with GivenWhenThen {    
  
  describe("A Singleton") {
    
    it("should return only one given value") {
      
      given("a Singleton")
      val randomInt = new Random(System.currentTimeMillis()).nextInt
      val streamable = Singleton(randomInt)
      
      then("it should not be infinite")
      assert(!streamable.isInfinite)
      
	    val stream = streamable.getStream
      and("the head of its stream should be the given value")	    
	    expect(randomInt) {
        stream.head
      } 
	    
      and("its tail should be empty")     
	    expect(Nil) { stream.tail }
      
      and("the similar should hold for its values")      
	    expect(1) {
        streamable.getValues.head
      } 
	    expect(Nil) { streamable.getValues.tail }
    }
    
  }
  
}