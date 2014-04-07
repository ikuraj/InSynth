package insynth
package enumeration
package benchmarks

import org.scalatest._
import org.scalameter.api._

import dependent._
import memoization._
import insynth.{ enumeration => e }
import insynth.util._

import insynth.util.logging._

class BinarySearchTreeBenchmark extends PerformanceTest.OfflineReport with HasLogger with ProfileLogger {
  import common._
  import Structures._
  import BSTrees._

  performance of "scife enumerators" in {
    measure method "Binary Search Trees" in {
      val sizes = Gen.range("size")(1, 15, 1)
      implicit val memScope = new MemoizationScope
      val enumerator = constructEnumerator

      using(sizes) curve ("Binary Search Trees") warmUp {
        for (size <- 1 to 10) {
          val tdEnumVal = enumerator
          val enum = enumerator.getEnum((size, 1 to size))
          for (i <- 0 until enum.size) enum(i)
        }
      } setUp { _ =>
        memScope.clear
      } in { (size: Int) =>
        val tdEnumVal = enumerator
        val enum = enumerator.getEnum((size, 1 to size))
        for (i <- 0 until enum.size) enum(i)
      }
    }
  }

  private def constructEnumerator(implicit ms: MemoizationScope) = {
    val rootProducer = Depend(
      (range: Range) => {
        e.WrapArray(range)
      })

    val sizeProducer = Depend(
      (size: Int) => {
        e.WrapArray(0 until size)
      })

    Depend.memoized(
      (self: Depend[(Int, Range), Tree], pair: (Int, Range)) => {
        val (size, range) = pair

        if (size <= 0) e.Singleton(Leaf)
        else if (size == 1) e.WrapArray(range map { v => Node(Leaf, v, Leaf) })
        else {
          val roots = rootProducer.getEnum(range)
          val leftSizes = sizeProducer.getEnum(size)

          val rootLeftSizePairs = e.Product(leftSizes, roots)

          val leftTrees: Depend[(Int, Int), Tree] = new InMap(self, { (par: (Int, Int)) =>
            val (leftSize, median) = par
            (leftSize, range.start to (median - 1))
          })

          val rightTrees: Depend[(Int, Int), Tree] =
            new InMap(self, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (size - leftSize - 1, (median + 1) to range.end)
            })

          val leftRightPairs: Depend[(Int, Int), (Tree, Tree)] =
            Product(leftTrees, rightTrees)

          val allNodes =
            memoization.Chain[(Int, Int), (Tree, Tree), Node](rootLeftSizePairs, leftRightPairs,
              (p1: (Int, Int), p2: (Tree, Tree)) => {
                val ((leftSize, currRoot), (leftTree, rightTree)) = (p1, p2)

                Node(leftTree, currRoot, rightTree)
              })

          allNodes
        }
      })
  }

}
