package insynth.enumeration
package dependent

import insynth.enumeration.lzy._

import insynth.util.logging._

class ConcatFinite[I, O]
  (s1: DependFinite[I, O], s2: DependFinite[I, O])
  extends DependFinite[I, O] with HasLogger {
  
  override type EnumType = Finite[O]
  
  override def getEnum(parameter: I) = {
    val left = s1.getEnum(parameter)
    val right = s2.getEnum(parameter)
    
    ConcatFinite(left, right)
  }
  
}