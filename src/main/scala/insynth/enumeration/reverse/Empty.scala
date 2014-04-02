package insynth.enumeration
package reverse

import _root_.insynth.{ enumeration => e }

object Empty extends e.Empty with Reverse[Nothing] {
  
  override def reverse[Nothing](a: Nothing) = 
    throw new UnsupportedOperationException("Cannot call reverse on an empty enumerator.")
  
}