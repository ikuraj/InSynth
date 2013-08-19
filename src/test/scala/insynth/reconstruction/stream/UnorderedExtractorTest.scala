package insynth.reconstruction

import insynth.structures.{ Function => FunctionType, _ }
import insynth.reconstruction.{ intermediate => int }
import insynth.reconstruction.{ stream => lambda }
import insynth.reconstruction.intermediate.IntermediateTransformer
import insynth.reconstruction.stream.{ Extractor, UnorderedStreamFactory }

import org.scalatest.junit.JUnitSuite
import org.junit.{ Test, Ignore }
import org.junit.Assert._

import insynth.common._
import insynth.testdomain.{ TestDeclaration => Declaration }

class UnorderedExtractorTest extends JUnitSuite {

  import CommonDeclarations._
  import CommonIntermediate._
  import CommonLambda._
  import CommonDomainTypes._
    
  val extractor = new Extractor(new UnorderedStreamFactory)
  
  import lambda.Node._
      
  @Test
  def treeReconstructBooleanToIntIntermediate: Unit = {  
    val extractorResults = extractor(constructBooleanToIntIntermediate) take 1
    
    assertEquals(1, extractorResults.size)
    
    for ( ((node, weight), lambdaNode) <- extractorResults zip constructBooleanToIntIntermediateLambda ) {	    
	    assertEquals(0f, weight, 0f)	  
	    assertEquals(lambdaNode, node)
    }
  }    
  
  @Test
  def leafTestIdent = {    
    val id1 = int.Identifier(typeInt, intDeclaration)
  				
    val extractorResults = extractor(id1) take 100
		
    assertEquals(1, extractorResults.size)
    assertEquals(id1: lambda.Node, extractorResults.head._1)
  }
  
  @Test
  def leafTestVar = {  		
		val vl1 = int.Variable(typeInt, "val1")
		
    val extractorResults = extractor(vl1) take 100
		
    assertEquals(1, extractorResults.size)
    assertEquals(vl1: lambda.Node, extractorResults.head._1)
  }
  
  @Test
  def abstractionTest = {    
	  val intDeclaration2 = Declaration(
	    typeInt, "dec2"
	  )
	  val intDeclaration3 = Declaration(
	    typeInt, "dec3"
	  )
    val id1 = int.Identifier(typeInt, intDeclaration)
    val id2 = int.Identifier(typeInt, intDeclaration2)
    val id3 = int.Identifier(typeInt, intDeclaration3)
    
    val val1 = int.Variable(typeInt, "val1")
  		
		val abs = int.Abstraction(
	    FunctionType(List(typeInt), typeInt),
	    List(val1),
	    Set(id1, id2, id3)
    )
		
    val extractorResults = extractor(abs) take 100
		
    assertEquals(3, extractorResults.size)
    assertEquals(extractorResults(0)._1,
        lambda.Abstraction(
			    FunctionType(List(typeInt), typeInt),
			    List(val1),
			    id1
		    )
    )
    assertEquals(extractorResults(1)._1,
        lambda.Abstraction(
			    FunctionType(List(typeInt), typeInt),
			    List(val1),
			    id2
		    )
    )
    assertEquals(extractorResults(2)._1,
        lambda.Abstraction(
			    FunctionType(List(typeInt), typeInt),
			    List(val1),
			    id3
		    )
    )
  }
  
  @Test
  def treeIntToIntRec: Unit = {
    
    val intermediateTree = constructIntToIntIntermediate
            
    def confirmResults(num: Int) = {
	    val extractorResults = extractor(intermediateTree) take num	    
	    assertEquals(num, extractorResults.size)
	    
	    for ( ((node, _), lambdaNode) <- extractorResults zip constructIntToIntIntermediateFirstLambda(num))		  
		    assertEquals(node, lambdaNode)	    
    }
    
    for (ind <- 1 to 5) confirmResults(ind)    
  }
  
  
  @Test
  def treeIntAndBoolToIntIntermediate = {
    val intermediateTree = constructIntToIntIntermediateBoth
            
    def confirmResults(num: Int) = {
      // take two times this number of elements because we have two roots of recursion
      // take two times more to be sure that extractor extracts needed trees (node the non-determinism)
	    val extractorResults = (extractor(intermediateTree) take (num * 2 * 2)) map { _._1 }	    
	    assertEquals(num * 4, extractorResults.size)
	    	  
	    val message = "Extracted " + extractorResults.zipWithIndex.map(p => p._2 + ": " + p._1).mkString("\n")
	    
	    for (node <- constructIntAndBoolToIntIntermediateLambda(num))
	    	assertTrue(node + " is not extracted.\n" + message, extractorResults contains node)	    
    }
    
    for (ind <- 1 to 5) confirmResults(ind)    
  }

}