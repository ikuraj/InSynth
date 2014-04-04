package insynth.enumeration
package reverse
package dependent

import insynth.enumeration.{ combinators => ecomb }

import insynth.util.logging._

object Product {
  
  def apply[I, O1, O2](s1: DependReverse[I, O1], s2: DependReverse[I, O2]) = {
  	new ProductFinite(s1, s2) with DependReverse[I, (O1, O2)]
  }
  
}