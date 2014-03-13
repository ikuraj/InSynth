package insynth.streams

import scala.collection.mutable.MutableList

class MemoizationScope {
  
  val memoizations = MutableList[Memoizable]()
  
  def add(m: Memoizable) = m +=: memoizations 
  
  def clear = for (m <- memoizations) m.clearMemoization

}