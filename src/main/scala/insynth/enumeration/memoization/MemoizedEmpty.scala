package insynth.enumeration
package memoization

object MemoizedEmpty extends Empty with Memoizable {
    
  override def clearMemoization { }

}