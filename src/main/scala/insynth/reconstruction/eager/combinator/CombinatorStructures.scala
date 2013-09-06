package insynth.reconstruction.eager.combinator

import scala.math.min

import insynth.load.Declaration
import insynth.{ structures => InSynth }
import insynth.structures.{ SuccinctType => Type, _ }

import insynth.util.logging.HasLogger
import insynth.util.format.{ FormatSuccinctNode => FormatNode, _ }

// TODO set required combinations in each Tree node after we hit the limit in the top
// tree
// TODO shared and private parameters of abstractions (not to explore two trees that
// are the same and shared between abstractions)
// TODO refactor Tree (subclass normal and top tree)
// TODO isDone and isPruned sometimes redundant

/**
 * dictates some rules about the combination search and declares loggers
 */
object Rules {  
  // if the pruning started
  var doPruning = false

  val BottomType = Atom(InSynth.BottomType)
}

/**
 * represents a node in a combinator tree that knows how many tree
 * combinations it can yield
 */
trait Combinations {
  def getNumberOfCombinations: Int
  
  // pruned tells us if the combination search should neglect the search down this node
  var pruned: Boolean = false
  def isPruned = pruned
  def setPruned(valPruned: Boolean):Unit = { this.pruned = valPruned }  
  
  // is the node ready to transform
  def isDone: Boolean
  
  // get weight value with respect to the traversal
  def getTraversalWeight: Float
  
  // get weight value of the computed tree
  def getMinComputedWeight: Float
}

/**
 * represents a node in the proof tree
 * this is the entity that is put in the priority queue to perform the search
 */
abstract class Expression(
    associatedTree: Tree, associatedNode: InSynth.Node
)
extends Combinations with Ordered[Expression] with HasLogger {
  // comparison of expression is done by their weights
  def compare(that:Expression) = {
    val thatVal = that.getTraversalWeight
      if (getTraversalWeight < thatVal) -1
      else if (getTraversalWeight > thatVal) 1
      else 0
  }
    
  // getters
  def getAssociatedTree = associatedTree
  def getAssociatedNode = associatedNode
  
  // reconstruct the output tree node
  def toTreeNode: Node
  
  // traversal weight returns the weight of associated tree
  override def getTraversalWeight = {
    associatedTree.getTraversalWeight
  }
  
  // for set operations
//  override def equals(o: Any) = o match {
//    case that: Expression => that.getAssociatedNode.equals(this.getAssociatedNode)
//    case _ => false
//  }
//  override def hashCode = getAssociatedNode.hashCode    
} 

/**
 * represents some parameter in the synthesized expression
 * it can have multiple alternatives to synthesize 
 */
class Tree(parent: Composite, val tpe: Type, var decls: Set[Expression] = Set())
extends Combinations with HasLogger
{
  // minimal weight of synthesized expression (used for pruning expressions with
  // larger weight)
  var minWeight = Float.MaxValue
  
  // add declaration to this tree (when it is explored)
  def addDeclaration(dec: Expression) {
    warning("Adding declaration " + dec + " to pruned tree " + this)
    decls += dec
  }
  
  // getters
  def getDeclarations:Set[Expression] = decls
  def getParent = parent
    
  // this method is called when some associated expression is explored
  def childDone(decl: Expression):Unit = {
	entering(this.getClass.getName, "childDone")
	info("Tree(" + this + ") received decl with weight " +
      decl.getMinComputedWeight + "and has minWeight " + minWeight)
    minWeight = min(getTraversalWeight + decl.getMinComputedWeight, minWeight)
    parent.childDone(this)
  }
  
  override def isDone =
    // just check if minimal weight has changed (is lower than the max double value)
    minWeight != Float.MaxValue// && !isPruned
  
  override def getNumberOfCombinations =
    (0 /: decls ) { (comb, decl) => comb + decl.getNumberOfCombinations }
    //(0 /: (decls filter { _.isDone }) ) { (comb, decl) => comb + decl.getNumberOfCombinations }
      
  override def setPruned(valPruned: Boolean):Unit = {
    super.setPruned(valPruned)
    for (dec <- decls; if !dec.isPruned) {
      dec.setPruned(valPruned)
    }

    if (!isDone && parent.isDone)
      warning("Pruning (" + tpe.toString + ") but its parent is done.")
    
  }
  	
  def toTreeNode = {
	entering(getClass.getName, "toTreeNode")
	fine("toTreeNode started on Tree: " + tpe.toString )
    
    // transform only those expressions that are done
    val declsToTransform = decls filter { _.isDone }
    
    info("has doneDeclarations " + declsToTransform.size)
    if (declsToTransform.isEmpty)
      warning("Tree (" + tpe.toString + "): toTree has none done expressions "
	    + "(declsToTransform: " + ("" /: declsToTransform){ (s, t) => s + ", "  + FormatNode(t.getAssociatedNode) } + ")")
    
    // transform declarations associated with this node
    val nodeSet = (Set[Node]() /: declsToTransform) {
      (set, dec) => {
        // transform declaration only if it is not prune
        set + dec.toTreeNode
//    	if (!dec.isPruned) set + dec.toTreeNode
//    	else set
      }
    }
    
    if (nodeSet.isEmpty)
      warning("Tree (" + tpe.toString + "): toTree transformed nodes set empty")
    
    // set of transformed nodes should not be empty
    assert(!nodeSet.isEmpty)
    //return a container node with the set of transformed nodes
  	ContainerNode(nodeSet)
  }
  
  override def toString =
    "Tree(" + 
    ("" /: decls ) { 
	  (string, dec) => { string + "," + FormatNode(dec.getAssociatedNode, 0) }
  	} + ")"
  	
  // traversal weight corresponds to the one of the parent (composite)	
  override def getTraversalWeight = parent.getTraversalWeight
  
  // minimum computed weight for this tree is the minWeight
  override def getMinComputedWeight = minWeight
  	
  // check if the weight is pruned at this tree
  // a method that can speed up pruning (if weight at the start is greater than minWeight
  // of some parent tree up the hierarchy)
  def checkIfPruned(weight: Float): Boolean = {
  	if (weight > minWeight)
  	  info("At node(" + this + ") checkIfPruned succeded (" + weight + ">" + minWeight + ")")

    if (weight > minWeight) true
  	else parent.associatedTree.checkIfPruned(weight)
  }
}

/**
 * tree that is at the top of the hierarchy (if its child is done, the tree is
 * has some expressions explored) 
 */
class TopTree(neededCombinations: Int)
extends Tree(null, BottomType)
{    
  override def childDone(decl: Expression):Unit = {
    info("Child done at top tree called.")
    if (neededCombinations <= getNumberOfCombinations) {
      info("Yes we found enough combinations(" + getNumberOfCombinations + ", will start pruning.")
      	
      Rules.doPruning = true
      //Rules.logger.info("Tree looks like: " + FormatCombinations(this))
    }
  }
  
  override def checkIfPruned(weight: Float): Boolean = {
		false
  }
  
  override def getTraversalWeight = 0
}

/**
 * corresponds to a declaration
 */
case class Composite(
    associatedTree: Tree, origDecl: Declaration, associatedNode: InSynth.SimpleNode
) 
extends Expression(associatedTree, associatedNode) {
  var children: Set[Tree] = Set()
  var doneChildren: Set[Tree] = Set()
  
  def addChild(decl: Tree) = {
    fine("Added child " + decl + " to composite " + origDecl.getSimpleName)
    
    children += decl 
  }
  
  def childDone(decl: Tree):Unit = {
    for (child <- doneChildren)
      assert(child.getMinComputedWeight < Float.MaxValue)
    
    doneChildren += decl
    // if my weight is larger or equal then prune my sub-tree
    // NOTE after the point of enough combinations, accepts only better nodes
    // NOTE we can get a call to childDone again but minWeight will be the same as
    // previously set getWeight (we need >)
    if (Rules.doPruning && associatedTree.getTraversalWeight + getMinComputedWeight > associatedTree.minWeight && !isPruned) {
      info("Pruning Composite (" + FormatNode(associatedNode, 0) + ")")
    	
      // mark the node as pruned
	  setPruned(true)
	  // do not add done child up the hierarchy
    }
    else {
      assert(children.size >= doneChildren.size)    
      if ((children &~ doneChildren).isEmpty) {
	    // this one is done
    	associatedTree.childDone(this)
      }
    }
  }  
  
  override def isDone =
    // check if all children are done
    !children.isEmpty && (children &~ doneChildren).isEmpty// && !isPruned
  
  def getNumberOfCombinations: Int =
    if (!isDone) 0
    else (1 /: children) { (comb, decl) => comb * decl.getNumberOfCombinations }
    
  override def setPruned(valPruned: Boolean):Unit = {
    if (associatedTree.minWeight >= getTraversalWeight && !associatedTree.isPruned)
      warning("Pruning (" + FormatNode(associatedNode, 0) + ") but it has the min weight at associated tree.")

    super.setPruned(valPruned)
    for (tree <- children; if !tree.isPruned) {
      tree.setPruned(valPruned)
    }
  }
  
  override def toTreeNode = {
	entering(getClass.getName, "toTreeNode")
	fine("toTreeNode started on Composite: " + origDecl.getSimpleName)
	if (!(children &~ doneChildren).isEmpty) {
	  warning("Composite " + origDecl.getSimpleName + " toTree has not all children done "
	    + "(children: " + ("" /: children){ (s, t) => s + ", "  + t.tpe.toString } 
	  	+ ", doneChildren: " + ("" /: doneChildren){ (s, t) => s + ", "  + t.tpe.toString } + ")")
	  warning("The toTreeNode failed composite has " + getNumberOfCombinations + " combinations.")
	}
    
    // this assert is needed since transform should not be called if node is not done
    assert((children &~ doneChildren).isEmpty)
    // return simple node
    SimpleNode(
      List(origDecl), associatedTree.tpe,
      (Map[Type, ContainerNode]() /: doneChildren) {
        (map, tree) => {
          map + (tree.tpe -> tree.toTreeNode)
        }
      }
    )
  }
    
  override def getTraversalWeight = {
    super.getTraversalWeight + origDecl.getWeight
  }
  
    // return weight as sum of geWeight in super class and weights of all children
  override def getMinComputedWeight =
    (origDecl.getWeight /: doneChildren) {
      (sum, child) => sum + child.minWeight
    }
  
  override def toString =
    "Composite(" + origDecl.getSimpleName + ":" + getTraversalWeight + ")"
}

/**
 * corresponds to a leaf node of a declared identifier
 */
case class Simple(
    associatedTree: Tree, origDecl: Declaration,
    associatedNode: InSynth.SimpleNode
)
extends Expression(associatedTree, associatedNode) {
  def getNumberOfCombinations: Int = 1
  
  override def toTreeNode = {
    entering(getClass.getName, "toTreeNode")
    fine("toTreeNode started on: " + origDecl.getSimpleName)
    
    SimpleNode(
      List(origDecl), associatedTree.tpe, Map[Type, ContainerNode]()
    )
  }
  
  override def isDone = true//!isPruned
  
  override def getTraversalWeight = {
    super.getTraversalWeight + origDecl.getWeight
  }
  
  override def getMinComputedWeight = origDecl.getWeight
  
  override def toString =
    "Simple(" + origDecl.getSimpleName + ":" + getTraversalWeight + ")"
}

/**
 * corresponds to a leaf node which is an expression from context
 */
case class LeafExpression(associatedTree: Tree, weight: Float, associatedNode: InSynth.SimpleNode)
extends Expression(associatedTree, associatedNode) {
  def getNumberOfCombinations: Int = 1
  
  override def toTreeNode = {
    entering(getClass.getName, "toTreeNode")    
    fine("toTreeNode started on: " + FormatNode(associatedNode, 0))
    
    AbsNode(associatedNode.getDecls)
  }
  
  override def isDone = true//!isPruned
  
  override def toString =
    "LeafExpression(" + getTraversalWeight + ")"
    
  override def getTraversalWeight = {
    super.getTraversalWeight + weight
  }
  
  override def getMinComputedWeight = weight
}