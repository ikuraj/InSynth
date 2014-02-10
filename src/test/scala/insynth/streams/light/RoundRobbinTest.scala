package insynth.streams
package light

import org.scalatest._

class RoundRobbinTest extends FunSuite {
  
  import Utils._
  
  test("simple") {
    val rr = RoundRobbin(Array(
      WrapperArray(1, 2, 3),
      WrapperArray(4, 5, 6),
      WrapperArray(7, 8, 9)
    ))
    
    (0 until 9).map(
      rr(_)
    ) should be ( List(1, 4, 7, 2, 5, 8, 3, 6, 9) )
    
  }
  
}