package insynth
package streams
package dependent

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.matchers._

import streams.{ light => e }

import util._
import util.format._
import util.logging._
import common._

class BSTWithCombinatorsTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
	HasLogger with ProfileLogger {  
  import Checks._
  import Structures._
  import BSTrees._
  
  def bstTests(trees: Dependent[(Int, Range), Tree]) {
    object OMG {
      var _res: light.Enum[Tree] = null
      var elements: Seq[Tree] = null
      def res = _res
      def res_=(n: light.Enum[Tree]) = {
        _res = n
        elements = (0 until res.size) map { res(_) }
      }
      def clue = (0 until res.size).map(res(_)).mkString(",")
    }
    import OMG._
    
    val profileRange = 9 to 9
    
    withLazyClue("Elements are: " + clue) {
//      for (size <- 1 to 3) {
//        res = trees.getStream((size, Range(size, size - 1)))
//        res.size should be (0)
//        elements should be ('empty)
//        
//        res = trees.getStream((0, 1 to size))
//        res(0) should be (Leaf)
//        res.size should be (1)
//      }
//
//      res = trees.getStream(1, 1 to 3)
//      res.size should be (3)
//      elements should contain theSameElementsAs (1 to 3).map(
//        Node(Leaf, _, Leaf)
//      )
//
//      res = trees.getStream(2, 1 to 2)
//      res.size should be (2)
//      elements should contain allOf (
//        Node(Leaf, 1, Node(Leaf, 2, Leaf)),
//        Node(Node(Leaf, 1, Leaf), 2, Leaf)
//      )
//
//      res = trees.getStream(3, 1 to 3)
//      res.size should be (5)
//      elements should contain allOf (
//        Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
//        Node(Leaf, 1, Node(Node(Leaf, 2, Leaf), 3, Leaf))
//      )
//
//      res = trees.getStream(3, 1 to 4)
//      res.size should be (5 * Binomial.binomialCoefficient(4, 3))
//      elements should contain allOf (
//        Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
//        Node(Leaf, 1, Node(Node(Leaf, 2, Leaf), 3, Leaf))
//      )

      for (size <- profileRange) {
        profile("Getting stream for BST of size %d".format(size)) {
          res = trees.getStream(size, 1 to size)
        }
        profile("Claculating size for BST of size %d".format(size)) {
          res.size should be (Catalan.catalan(size))
        }
        profile("Getting elements for BST of size %d".format(size)) {
          for (ind <- 0 until res.size) res(ind)
        }
        
        assert( (for (ind <- 0 until res.size) yield res(ind)).forall( invariant(_) ) )
      }
      
    }

    // if size > range.size then no element should be generated
    forAll ("size", "range max", maxSize(10)) { (s: Int, m: Int) =>
      whenever (m < s) {
        trees.getStream(s, 1 to m).size should be (0)
      }
    }
    
  }

  test("binary search trees") {
    
    val maxLength = 10
    
    val rootProducer = Producer[Range, Int](
      (range: Range) => {
        e.WrapperArray( range )
      }
    )
    
    val sizeProducer = Producer[Int, Int](
      (size: Int) => {
        e.WrapperArray( 0 until size )
      }
    )
    
    var getTreeOfSize: Dependent[ (Int, Range), Tree ] = null
    
    val treesOfSize: Dependent[ (Int, Range), Tree ] = Producer.memoized(
      ( pair: (Int, Range) ) => {
        val (size, range) = pair
        assert(size >= 0, "size=%d, range=%s" format (size, range))

        // do not care about the range, size is important (rangeProduced can return Empty)
        if (size <= 0) e.Singleton( Leaf )
        else if (size == 1) e.WrapperArray( range map { v => Node(Leaf, v, Leaf) } )
        else {
          val roots = rootProducer.getStream(range)
          val leftSizes = sizeProducer.getStream(size)
          
          val rootLeftSizePairs = e.Binary(leftSizes, roots)
          
//          val forBothTreesPairs = e.Mapper(rootLeftSizePairs, { (p: (Int, Int)) =>
//            val (root, leftSize) = p
//            (root,
//              leftSize - 1, range.start to (root - 1),
//              size - leftSize - 1, (root + 1) to range.end
//            )
//          })
          
          val leftTrees: Dependent[(Int, Int), Tree] = new InMapper(getTreeOfSize, { (par: (Int, Int)) =>
            val (leftSize, median) = par
            (leftSize, range.start to (median - 1))
          })
          
          val rightTrees: Dependent[(Int, Int), Tree] =
            new InMapper(getTreeOfSize, { (par: (Int, Int)) =>
	            val (leftSize, median) = par
	            (size - leftSize - 1, (median + 1) to range.end)
	          })
          
          val leftRightPairs: Dependent[(Int, Int), (Tree, Tree)] =
            CoupledBinary(leftTrees, rightTrees)
          
          import BinaryFiniteMemoized._
          
          val allNodes =
	      		combine[(Int, Int), (Tree, Tree), Node](rootLeftSizePairs, leftRightPairs,
//	    		    (leftSize: Int, mid: Int) => (size - 1, range.start to (mid - 1)),
	    		    (p1: (Int, Int), p2: (Tree, Tree)) => {
	    		      val ((leftSize, currRoot), (leftTree, rightTree)) = (p1, p2)

		      			assert( ! (size >= 2 && leftSize == 0 && size - leftSize - 1 == 0) )
		      			assert( ! (size >= 2 && leftTree == Leaf && rightTree == Leaf ) )
	    		      assert( !(leftSize > 0 && leftTree == Leaf), "leftSize=%d, leftTree=Leaf".format(leftSize))
	    		      Node( leftTree, currRoot, rightTree )
	    		    }
	  		    )
          
  		    allNodes
        }
      }
    )
    
    getTreeOfSize = treesOfSize
    
    bstTests( treesOfSize )
  }

  test("binary search trees, additional condition") {
    
    val maxLength = 10
    
    val rootProducer = Producer[Range, Int](
      (range: Range) => {
        e.WrapperArray( range )
      }
    )
    
    val sizeProducer = Producer[Int, Int](
      (size: Int) => {
        e.WrapperArray( 0 until size )
      }
    )
    
    var getTreeOfSize: Dependent[ (Int, Range), Tree ] = null
    
    val treesOfSize: Dependent[ (Int, Range), Tree ] = Producer.memoized(
      ( pair: (Int, Range) ) => {
        val (size, range) = pair
        assert(size >= 0, "size=%d, range=%s" format (size, range))

        if (range.size < 0) e.Empty
        else if (size == 0) e.Singleton( Leaf )
//        else if (size == 1) e.WrapperArray( range map { v => Node(Leaf, v, Leaf) } )
        else {
          val roots = rootProducer.getStream(range)
          val leftSizes = sizeProducer.getStream(size)
          
          val rootLeftSizePairs = e.Binary(leftSizes, roots)
          
//          val forBothTreesPairs = e.Mapper(rootLeftSizePairs, { (p: (Int, Int)) =>
//            val (root, leftSize) = p
//            (root,
//              leftSize - 1, range.start to (root - 1),
//              size - leftSize - 1, (root + 1) to range.end
//            )
//          })
          
          val leftTrees: Dependent[(Int, Int), Tree] = new InMapper(getTreeOfSize, { (par: (Int, Int)) =>
            val (leftSize, median) = par
            (leftSize, range.start to (median - 1))
          })
          
          val rightTrees: Dependent[(Int, Int), Tree] =
            new InMapper(getTreeOfSize, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (size - leftSize - 1, (median + 1) to range.end)
            })
          
          val leftRightPairs: Dependent[(Int, Int), (Tree, Tree)] =
            CoupledBinary(leftTrees, rightTrees)
          
          import BinaryFiniteMemoized._
          
          val allNodes =
            combine[(Int, Int), (Tree, Tree), Node](rootLeftSizePairs, leftRightPairs,
//              (leftSize: Int, mid: Int) => (size - 1, range.start to (mid - 1)),
              (p1: (Int, Int), p2: (Tree, Tree)) => {
                val ((leftSize, currRoot), (leftTree, rightTree)) = (p1, p2)

                assert( ! (size >= 2 && leftSize == 0 && size - leftSize - 1 == 0) )
                assert( ! (size >= 2 && leftTree == Leaf && rightTree == Leaf ) )
                assert( !(leftSize > 0 && leftTree == Leaf), "leftSize=%d, leftTree=Leaf".format(leftSize))
                Node( leftTree, currRoot, rightTree )
              }
            )
          
          allNodes
        }
      }
    )
    
    getTreeOfSize = treesOfSize
    
    bstTests( treesOfSize )
  }
  
}
