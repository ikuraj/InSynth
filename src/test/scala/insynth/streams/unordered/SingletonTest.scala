package insynth.streams.unordered

import scala.util.Random
import org.scalatest.GivenWhenThen
import insynth.streams.Streamable
import org.scalatest.funspec.AnyFunSpec

class SingletonTest extends AnyFunSpec with GivenWhenThen {
  
  describe("A Singleton") {
    
    it("should return only one Given value") {
      
      Given("a Singleton")
      val randomInt = new Random(System.currentTimeMillis()).nextInt
      val streamable: Streamable[Int] = Singleton(randomInt)
      
      Then("it should not be infinite")
      assert(!streamable.isInfinite)
      
	    val stream = streamable.getStream
      And("the head of its stream should be the Given value")	    
	    assertResult(randomInt) {
        stream.head
      } 
	    
      And("its tail should be empty")     
	    assertResult(Nil) { stream.tail }
    }
    
  }
  
}