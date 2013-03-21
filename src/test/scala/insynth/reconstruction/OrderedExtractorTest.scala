package insynth.reconstruction

import insynth.leon.loader.DeclarationFactory
import insynth.leon.{ ImmediateExpression }

import insynth.reconstruction.{ intermediate => int }
import insynth.reconstruction.{ stream => lambda }
import insynth.reconstruction.intermediate.IntermediateTransformer
import insynth.reconstruction.stream.{ Extractor, OrderedStreamFactory }

import leon.purescala.Definitions.{ FunDef, VarDecl, Program, ObjectDef }
import leon.purescala.Common.{ FreshIdentifier }
import leon.purescala.TypeTrees._
import leon.purescala.Trees.{ Variable => LeonVariable, _ }

import org.junit.{ Test, Ignore }
import org.junit.Assert._

import insynth.testutil.{ CommonIntermediate, CommonDeclarations, CommonLambda }

class OrderedExtractorTest {

  import CommonDeclarations._
  import CommonIntermediate._
  import CommonLambda._
  import DeclarationFactory._
    
  val extractor = new Extractor(new OrderedStreamFactory)
  
  val maxElToOutput = 20
  
  import lambda.Node._
  
  def assertWeight(lambdaNode: lambda.Node, weight: Float) =
    assertEquals(size(lambdaNode), weight, 0f)
    
  def assertWeight(expected: Int, weight: Float) =
    assertEquals(expected.toFloat, weight, 0f)	
    
  def assertWeight(pair: (lambda.Node, Float)) =
    assertEquals("Node " + pair._1, size(pair._1), pair._2, 0f)	    
    
  def assertTake(stream: Stream[(lambda.Node, Float)], num: Int) = {
    val result = stream take num
    val message = "Part of the resulting stream: " + result.take(maxElToOutput).mkString("\n")
    
    for (ind <- 0 until result.size)
      assertWeight(result(ind))
    for (ind <- 0 until result.size - 1)
      assertTrue("Weight are not in non-decreasing order.\n" + "At position " + ind + "\n" + message, stream(ind)._2 <= stream(ind + 1)._2)
    result
  }
      
  @Test
  def treeReconstructBooleanToIntIntermediate: Unit = {  
    val extractorResults = assertTake(extractor(constructBooleanToIntIntermediate), 1)
    
    assertEquals(1, extractorResults.size)
    
    for ( ((node, weight), lambdaNode) <- extractorResults zip constructBooleanToIntIntermediateLambda ) {
	    assertEquals(lambdaNode, node)	    
	    
	    assertWeight(lambdaNode, weight)	  
    }
  }    
  
  @Test
  def leafTestIdent = {
    val id1 = int.Identifier(Int32Type, intDeclaration)
  				
    val extractorResults = assertTake(extractor(id1), 100)
		
    assertEquals(1, extractorResults.size)
    assertEquals(id1: lambda.Node, extractorResults.head._1)
    
    assertWeight(id1: lambda.Node, extractorResults.head._2)	
  }
  
  @Test
  def leafTestVar = {  		
		val vl1 = int.Variable(Int32Type, "val1")
		
    val extractorResults = assertTake(extractor(vl1), 100)
		
    assertEquals(1, extractorResults.size)
    assertEquals(vl1: lambda.Node, extractorResults.head._1)

    assertWeight(vl1: lambda.Node, extractorResults.head._2)
  }
  
  @Test
  def abstractionTest = {    
	  val intDeclaration2 = makeDeclaration(
	      ImmediateExpression("2", IntLiteral(2)),
	      Int32Type
	  )
	  val intDeclaration3 = makeDeclaration(
	      ImmediateExpression("3", IntLiteral(3)),
	      Int32Type
	  )
    val id1 = int.Identifier(Int32Type, intDeclaration)
    val id2 = int.Identifier(Int32Type, intDeclaration2)
    val id3 = int.Identifier(Int32Type, intDeclaration3)
    
    val val1 = int.Variable(Int32Type, "val1")
  		
		val abs = int.Abstraction(
	    FunctionType(List(Int32Type), Int32Type),
	    List(val1),
	    Set(id1, id2, id3)
    )
		
    val extractorResults = assertTake(extractor(abs), 100)
		
    assertEquals(3, extractorResults.size)
    assertEquals(extractorResults(0)._1,
        lambda.Abstraction(
			    FunctionType(List(Int32Type), Int32Type),
			    List(val1),
			    id1
		    )
    )
    assertEquals(extractorResults(1)._1,
        lambda.Abstraction(
			    FunctionType(List(Int32Type), Int32Type),
			    List(val1),
			    id2
		    )
    )
    assertEquals(extractorResults(2)._1,
        lambda.Abstraction(
			    FunctionType(List(Int32Type), Int32Type),
			    List(val1),
			    id3
		    )
    )
        
    for (i <- 0 to 2)
      assertWeight(2, extractorResults(2)._2)
  }
  
  @Test
  def treeIntToIntRec: Unit = {
    
    val intermediateTree = constructIntToIntIntermediate
            
    def confirmResults(num: Int) = {
	    val extractorResults = assertTake(extractor(intermediateTree), num)   
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
	    val extractorResults = assertTake(extractor(intermediateTree), (num * 2 * 2)) map { _._1 }	    
	    assertEquals(num * 4, extractorResults.size)
	    	  
	    val message = "Extracted " + extractorResults.zipWithIndex.map(p => p._2 + ": " + p._1).mkString("\n")
	    
	    for (node <- constructIntAndBoolToIntIntermediateLambda(num))
	    	assertTrue(node + " is not extracted.\n" + message, extractorResults contains node)	    
    }
    
    for (ind <- 1 to 5) confirmResults(ind)    
  }

}