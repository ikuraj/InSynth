package insynth.reconstruction.eager.combinator

import scala.collection.mutable.PriorityQueue

import insynth.{ structures => InSynth }
import InSynth._

import insynth.util.logging._
import insynth.util.format.{ FormatSuccinctNode => FormatNode }

/**
 * object which application transforms an InSynth representation input
 * to the pruned tree representation
 */
object Combinator extends ((InSynth.SimpleNode, Int, Int) => Option[Node]) with HasLogger {
  
  /**
   * perform the transformation - apply with number of combinations needed, the resulting
   * pruned tree will encode at least this number of combinations
   * @param root root of the input proof tree
   * @param neededCombinations number of combinations needed
   * @param maximumTime maximal number of milliseconds the combinator should take
   * @return root node of the proof tree
   */
  def apply(root: InSynth.SimpleNode, neededCombinations: Int, maximumTime: Int): Option[SimpleNode] = {
	entering(getClass.getName, "apply")
	finest("Entering combinator step (root: "+ FormatNode(root, 6) + ", combinations: " + neededCombinations)
    
    // get time at the beginning of the combination step
    val startTime = System.currentTimeMillis
        
    // import transformer from InSynth to intermediate declaration
//    import DeclarationTransformer.fromInSynthDeclaration
    
    // reset so that pruning is not done
    Rules.doPruning = false
      
    // pair type that will be put in the priority queue
    // ( expression to be explored, a set of expressions visited on the path )
    type ExpressionPair = (Expression, Set[InSynth.Node])
    // priority queue
    var pq = new PriorityQueue[ExpressionPair]() (
      // ordering defined on the expressions  
      new Ordering[ExpressionPair] {                                                                    
	    def compare(a : ExpressionPair, b : ExpressionPair) = b._1.compare(a._1)                                           
      } 
    )    
    // declare root Tree which starts the hierarchy 
    val rootTree:Tree = new TopTree(neededCombinations)
    // a single declaration of the root Tree is the one corresponding to the root node
    val rootDeclaration = Composite(rootTree, root.getDecls.head, root)
        
    // add the root declaration and an empty set
    val startTuple = (rootDeclaration, Set[InSynth.Node]())
    pq += startTuple
    
    /* start the traversal */
    
    // while there is something in the queue
    while (!pq.isEmpty && (System.currentTimeMillis - startTime < maximumTime)) {
      
      // dequeue a pair
      val (currentDeclaration, visited) = pq.dequeue
          
      { // logging stuff
	      finer("Current declaration processed " + currentDeclaration)
	            
	      // if current declaration is pruned or already visited on this path, log
	      if (currentDeclaration.isPruned)
	        fine("Declaration with " + FormatNode(currentDeclaration.getAssociatedNode, 0) + " pruned")	      
	      if (visited.contains(currentDeclaration.getAssociatedNode))
	        fine("Stumbled upon a cycle (discarding the node: " + FormatNode(currentDeclaration.getAssociatedNode, 0) + ")")
	      	      
	      // additional pruning conditions
	      if (currentDeclaration.getAssociatedTree.isPruned)
	        fine("!Declaration " + currentDeclaration + " pruned")	      
	      if (Rules.doPruning && currentDeclaration.getAssociatedTree.checkIfPruned(currentDeclaration.getTraversalWeight))
	        fine("!!Declaration " + currentDeclaration + " pruned")	      
	      
	      if (Rules.doPruning)
	        fine("Pruning is on, pq.size=" + pq.size)
      } // end logging
        
      // if current declaration is pruned or already visited on this path, ignore it
      if (
          !visited.contains(currentDeclaration.getAssociatedNode) && !currentDeclaration.isPruned &&
          !currentDeclaration.getAssociatedTree.isPruned &&
          !(Rules.doPruning && currentDeclaration.getAssociatedTree.checkIfPruned(currentDeclaration.getTraversalWeight))
          ) {
      
    	  // add explored declaration to its associated tree as explored
	      currentDeclaration.getAssociatedTree addDeclaration(currentDeclaration)
          finer("Adding expression " + currentDeclaration.toString + " to its tree")
	      
	      // check the type of the current declaration
	      currentDeclaration match {
	        // Composite represents a declaration with children 
	        case c:Composite => {
	          // get all needed parameters
	          val paramList = c.origDecl.getType match {
	            case Arrow(TSet(list), _) => list
	            case _ => throw new RuntimeException
	          }          
	          // for each its parameter add child to the queue for later traversing
	          for (parameter <- paramList) {
	            // create a tree for a parameter and associate it with the current composite
	            val paramTree = new Tree(c, parameter)
	            c.addChild(paramTree)
	            
	            // for each child node in associated InSynth nodes for the parameter
	            for(node <- c.associatedNode.getParams(parameter).nodes) {
	              // according to the type of node
	              // create a declaration and insert the pair into the queue
	              
	              // if there are no parameters insert simple nodes
	              if (node.getParams.isEmpty){
	                  // for each of its declarations
	                  for (dec <- node.getDecls;
	                  // optimization - only consider those nodes that have less weight than
	                  // all trees up to the tree
	                  notConsider = Rules.doPruning && paramTree.checkIfPruned(dec.getWeight + paramTree.getTraversalWeight);

            		  logDummy = if (notConsider) info("Decl " + dec.getSimpleName + " is not considered")
            		    
	                  if !notConsider
            		  ) // for
	                  {
	                    	                    
	                    // new pair of simple expression and extended path
	                    val newPair = 
	                      (
                            Simple(paramTree, dec, node),
                            visited + c.getAssociatedNode
                		  )
	                      
	                    finer("Adding " + newPair._1 + " to the queue")
                        // add new pair to the queue 
                    	pq += newPair
	                  }	                
                  // if there are parameters insert composite nodes
	              } else {
	                  // for each of its declarations
	                  for (dec <- node.getDecls;
	                  // optimization - only consider those nodes that have less weight than
	                  // all trees up to the tree
	                  notConsider = Rules.doPruning && paramTree.checkIfPruned(dec.getWeight + paramTree.getTraversalWeight);

            		  logDummy = if (notConsider) info("Decl " + dec.getSimpleName + " is not considered")

	                  if !(notConsider)
            		  ) // for 
	                  {
	                    // new pair of simple expression and extended path
	                    val newPair = 
	                      (
                            Composite(paramTree, dec, node), 
	                        visited + c.getAssociatedNode
                		  )
	                      	                      
	                    finer("Adding " + newPair._1 + " to the queue")
	                    
                        // add new pair to the queue 
	                    pq += newPair
	                  }	                
	              }
	            }
	          }
	        }
	        // Simple has no children 
	        case s:Simple =>
	          // mark it as done
	          s.associatedTree.childDone(s)
            // should not happen we do not deal with these anymore
	        case l: LeafExpression => throw new RuntimeException
//	        case l:LeafExpression => l.associatedTree.childDone(l)
	      }
      }
    }
        
	
    if (rootDeclaration.isPruned) severe("Root declaration is pruned!")	    
    if (!rootDeclaration.isDone) severe("Root declaration is not done!")	    
    finest("End of apply, reconstruction structures are: " + FormatCombinations(rootDeclaration) )
    info("Number of combinations found: " + rootDeclaration.getNumberOfCombinations )	    
    exiting(getClass.getName, "apply")
    
    
    if (rootDeclaration.isDone) {
	    // return transformed pruned tree as a result
	    val result = rootDeclaration.toTreeNode
	    
    	fine("Returning from apply with result: " + FormatPrNode(result) )
	    	    
	    // return the root node of the pruned tree
	    Some(result)
    } else None
  }
  
}