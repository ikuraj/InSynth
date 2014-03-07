package insynth
package streams
package dependent

import org.scalatest._
import org.scalatest.matchers._

import streams.{ light => e }

import util._
import util.format._
import util.logging._
import common._

class BSTWithCombinatorsTest extends FunSuite with Matchers with
	HasLogger with ProfileLogger {  
  import Checks._

  test("binary search trees") {
    import Structures._
    import BSTrees._
    
    val maxLength = 10
    
    val rootProducer = Producer[Range, Int](
      (range: Range) => {
        e.WrapperArray( range )
      }
    )
    
    val sizeProducer = Producer[Int, Int](
      (size: Int) => {
        e.WrapperArray( 0 to size )
      }
    )
    
    var getTreeOfSize: Dependent[ (Int, Range), Tree ] = null
    var getTreeOfSizes: Dependent[ (Int, Range), (Tree, Int) ] = null
    
    val treesOfSize: Dependent[ (Int, Range), Tree ] = Producer.memoized(
      ( pair: (Int, Range) ) => {
        val (size, range) = pair
        assert(size >= 0)

        // do not care about the range, size is important (rangeProduced can return Empty)
        if (size <= 0) e.Singleton( Leaf )
        else if (size == 1) e.WrapperArray( range map { v => Node(Leaf, v, Leaf) } )
        else {
          val roots = rootProducer.getStream(range)
          val leftSizes = sizeProducer.getStream(size)
          
          val rootLeftSizePairs = e.Binary(roots, leftSizes)
          
//          val forBothTreesPairs = e.Mapper(rootLeftSizePairs, { (p: (Int, Int)) =>
//            val (root, leftSize) = p
//            (root,
//              leftSize - 1, range.start to (root - 1),
//              size - leftSize - 1, (root + 1) to range.end
//            )
//          })
          
          val leftTrees = new InMapper(getTreeOfSize, { (par: (Int, Int)) =>
            val (leftSize, root) = par
            (leftSize - 1, range.start to root)
          })
          
          val rightTrees = new InMapper(getTreeOfSize, { (par: (Int, Int)) =>
            val (leftSize, root) = par
            (size - leftSize - 1, (root + 1) to range.end)
          })
          
          val leftRightPairs = BinaryPairs(leftTrees, rightTrees)
          
          import BinaryFiniteMemoized._
          
          val allCombined =
	      		chainCombined(forBothTreesPairs, leftRightPairs,
	    		    (mid: Int) => (size - 1, range.start to (mid - 1)),
	    		    (currRoot: Int, leftPair: (Tree, Int)) => {
	    		      val (leftTree: Tree, leftSize: Int) = leftPair
	    		      assert( !(leftSize > 0 && leftTree == Leaf), "leftSize=%d, leftTree=Leaf".format(leftSize))
	    		      ( leftTree, leftSize, currRoot )
	    		    }
	  		    )
          
      		chainCombined(leftTrees, getTreeOfSize,
    		    (tuple: (Tree, Int, Int)) => {
    		      val (_, leftSize, root) = tuple
    		      assert( ! (size >= 2 && leftSize == 0 && size - leftSize - 1 == 0) )
    		      (size - leftSize - 1, (root + 1) to range.end)
    		    },
    		    (leftTuple: (Tree, Int, Int), rightTree: Tree) => {
    		      val (leftTree, _, root) = leftTuple
    		      
    		      assert( ! (size >= 2 && leftTree == Leaf && rightTree == Leaf ),
  		          "leftSize=%d (Leaf), rightSize=%d (Leaf)"
    		          .format(leftTuple._2, size - leftTuple._2 - 1) )
    		      Node(leftTree, root, rightTree)
    		    }
  		    )
        }
      }
    )
    
    getTreeOfSize = treesOfSize
    getTreeOfSizes = treesUpToSize
    
    val trees = treesOfSize
    
    object OMG {
	    var _res: light.Enumerable[Tree] = null
	    var elements: Seq[Tree] = null
	    def res = _res
	    def res_=(n: light.Enumerable[Tree]) = {
	      _res = n
	  		elements = (0 until res.size) map { res(_) }
	    }
	    def clue = (0 until res.size).map(res(_)).mkString(",")
    }
    import OMG._
    
    withLazyClue("Elements are: " + clue) {
      for (size <- 1 to 3) {
	      res = trees.getStream((size, Range(size, size - 1)))
	      res.size should be (0)
	      elements should be ('empty)
	      
	      res = trees.getStream((0, 1 to size))
	      res(0) should be (Leaf)
	      res.size should be (1)
      }

      res = trees.getStream(1, 1 to 3)
      res.size should be (3)
      elements should contain theSameElementsAs (1 to 3).map(
    		Node(Leaf, _, Leaf)
      )

      res = trees.getStream(2, 1 to 2)
      res.size should be (2)
      elements should contain allOf (
    		Node(Leaf, 1, Node(Leaf, 2, Leaf)),
    		Node(Node(Leaf, 1, Leaf), 2, Leaf)
      )

      res = trees.getStream(3, 1 to 3)
      res.size should be (5)
      elements should contain allOf (
    		Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
    		Node(Leaf, 1, Node(Node(Leaf, 2, Leaf), 3, Leaf))
      )

      res = trees.getStream(3, 1 to 4)
      res.size should be (5 * Binomial.binomialCoefficient(4, 3))
      elements should contain allOf (
    		Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
    		Node(Leaf, 1, Node(Node(Leaf, 2, Leaf), 3, Leaf))
      )

      for (size <- 10 to 10) {
	      profile("Getting stream for BST of size %d".format(size)) {
	      	res = trees.getStream(size, 1 to size)
	      }
	      profile("Claculating size for BST of size %d".format(size)) {
	      	res.size should be (Catalan.catalan(size))
	      }
	      profile("Getting elements for BST of size %d".format(size)) {
		      for (ind <- 0 until res.size) res(ind)
	      }
      }
      
    }
  }
  
}
