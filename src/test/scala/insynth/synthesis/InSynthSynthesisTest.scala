package insynth

import scala.util.Random

import insynth.leon.HoleFinder
import insynth.leon.loader.LeonLoader

import _root_.leon.purescala.Trees.{ Hole, IntLiteral }
import _root_.leon.purescala.TreeOps
import _root_.leon.evaluators._
import _root_.leon.LeonContext

import insynth.testutil.CommonUtils
import insynth.leon.loader.LeonLoader

import _root_.testutil.TestConfig

import org.junit.Assert._
import org.junit.{ Test, Ignore }

class InSynthSynthesisTest {

  import TestConfig._
  import CommonUtils._
  import TreeOps._
  
  val rnd = new Random(System.currentTimeMillis())

	@Test
	def testLocalScope = {
    val holeFinder = new HoleFinder(synthesisTestDir + "LocalScope.scala")
				
    holeFinder.extract match {
      case Some((prog, hole)) =>
        val inSynth = new InSynth(prog, hole, false)
        
        val snippets = inSynth.getExpressions.take(200)        
        val snippetString = "Snippets " + snippets.mkString(", ")        
        assertEquals(200, snippets.size)
        
        for(expr <- List("r", "Some(r).v"))
          assertTrue("Snippets do not contain " + expr + "\n" + snippetString, snippets.map(_.getSnippet.toString) contains expr)
        
      case _ =>
        fail("HoleFinder could not extract hole from the program")
    }
	}
  
	@Test
	def testLocalScopeOrdered = {
    val holeFinder = new HoleFinder(synthesisTestDir + "LocalScope.scala")
				
    holeFinder.extract match {
      case Some((prog, hole)) =>
        val inSynth = new InSynth(prog, hole, true)
        
        val snippets = assertTake(inSynth.getExpressions, (200))        
        val snippetString = "Snippets " + snippets.mkString(", ")        
        assertEquals(200, snippets.size)
        
        val listOfExpected = List(("r", 1f + 1), ("Some(r).v", 3f + 1))
        
        for((expr, weight) <- listOfExpected) {
          assertTrue("Snippets do not contain " + expr + "\n" + snippetString, snippets.map(_.getSnippet.toString) contains expr)
          assertEquals("Weight of " + expr + " does not match.", weight, snippets(snippets.map(_.getSnippet.toString).indexOf(expr)).getWeight, 0f)
        }
              
      case _ =>
        fail("HoleFinder could not extract hole from the program")
    }
	}
	
	@Test
	def testHole = {    
	  val numberOfSnippets = 25

    val holeFinder = new HoleFinder(synthesisTestDir + "Hole.scala")
				
    holeFinder.extract match {
      case Some((prog, hole)) =>
        val inSynth = new InSynth(prog, hole, false)
        
        val snippets = inSynth.getExpressions.take(numberOfSnippets)        
        val snippetString = "Snippets " + snippets.mkString(", ")    
        assertEquals(numberOfSnippets, snippets.size)
        
        // TODO test evaluator in a separate test ?
        val evaluator = new DefaultEvaluator(LeonContext(), prog)
        
        assertEquals(1, prog.definedFunctions.size)		
			  for (funDef <- prog.definedFunctions; val body = funDef.getBody)
			  	evaluator.eval(body, Map.empty) match {
				    case _: EvaluationSuccessful => fail("Evaluation result should not be okay")
				    case _ =>
			  	}
			  
			  for (snippet <- snippets; val snippetTree = snippet.getSnippet) {
			    assertTrue(prog.definedFunctions.head.hasBody)	    
			    val body = prog.definedFunctions.head.getBody
			    val newBody = searchAndReplaceDFS(
		        _ match {
		          case _: Hole => Some(snippetTree)
		          case _ => None
		        }
		      ) ( body )		      
		            	    
		      val randomValue = rnd.nextInt(10)
		      
		      assertEquals("Function arguments: " + prog.definedFunctions.head.args, 1, prog.definedFunctions.head.args.size)
		      val parIdent = prog.definedFunctions.head.args.head.id
		      
		      prog.definedFunctions.head.body = Some(newBody)
			  	evaluator.eval(newBody, Map(parIdent -> IntLiteral(randomValue))) match {
			      case _: EvaluationSuccessful | _: EvaluationFailure =>				     
				    case er => fail("Evaluation of " + newBody + " should not result in an error. Reason: " + er)
			  	}
		      prog.definedFunctions.head.body = Some(body)
			  }
        
        for(expr <- List("t", "method(t)"))
          assertTrue("Snippets do not contain " + expr + "\n" + snippetString, snippets.map(_.getSnippet.toString) contains expr)
        
      case _ =>
        fail("HoleFinder could not extract hole from the program")
    }
	  
	}
	
	@Test
	def testHoleOrdered = {    
	  val numberOfSnippets = 25

    val holeFinder = new HoleFinder(synthesisTestDir + "Hole.scala")
				
    holeFinder.extract match {
      case Some((prog, hole)) =>
        // avoid arithmetic operations
        val loader = new LeonLoader(prog, hole, false)
        val inSynth = new InSynth(loader, true)
        
        val snippets = inSynth.getExpressions.take(numberOfSnippets)        
        val snippetString = "Snippets " + snippets.mkString(", ")    
        assertEquals(numberOfSnippets, snippets.size)
        
        val listOfExpected = List(("t", 1f + 1), ("method(t)", 2f + 1), ("method(method(t))", 3f + 1))
        
        for((expr, weight) <- listOfExpected) {
          assertTrue("Snippets do not contain " + expr + "\n" + snippetString, snippets.map(_.getSnippet.toString) contains expr)
          assertEquals("Weight of " + expr + " does not match.", weight, snippets(snippets.map(_.getSnippet.toString).indexOf(expr)).getWeight, 0f)
        }
        
      case _ =>
        fail("HoleFinder could not extract hole from the program")
    }
	  
	}
	
	@Test
	def testSnippetsListOperations = {
    val holeFinder = new HoleFinder(synthesisTestDir + "LocalScope.scala")
				
    holeFinder.extract match {
      case Some((prog, hole)) =>
        val inSynth = new InSynth(prog, hole, false)
        
        val snippets = inSynth.getExpressions.take(100)        
        val snippetString = "Snippets " + snippets.mkString(", ")        
        assertEquals(100, snippets.size)
        
        for(expr <- List("r", "Some(r).v"))
          assertTrue("Snippets do not contain " + expr + "\n" + snippetString, snippets.map(_.getSnippet.toString) contains expr)
        
      case _ =>
        fail("HoleFinder could not extract hole from the program")
    }
	}
    
//	@Test
//	def testSnippetsListOperations3 = {
//    val holeFinder = new HoleFinder(synthesisTestDir + "ListOperationsHole2.scala")
//				
//    holeFinder.extract match {
//      case Some((prog, hole)) =>
//        val inSynth = new InSynth(prog, hole, false)
//        
//        val snippets = inSynth.getExpressions.take(50)        
//        val snippetString = "Snippets " + snippets.mkString(", ")        
//        assertEquals(50, snippets.size)
//        
//        for(expr <- List("r", "Some(r).v"))
//          assertTrue("Snippets do not contain " + expr + "\n" + snippetString, snippets.map(_.getSnippet.toString) contains expr)
//        
//      case _ =>
//        fail("HoleFinder could not extract hole from the program")
//    }
//				
//		(Globals.program, Globals.hole) match {
//		  case (Some(program), hole: Hole) => 		
//				val inSynth = new InSynth(program, hole)
//												
//				inSynth.getSnippets
//								
//        // update declarations
//        var allDeclarations = inSynth.allDeclarations
//        
//        println("all declarations: " + allDeclarations.mkString("\n"))
//                
//        // each variable of super type can actually have a subtype
//        // get sine declaration maps to be able to refine them
//        val directSubclassMap = Loader.directSubclassesMap        
//        println("directSubclassMap: " + directSubclassMap)      
//        println("directSubclassMap ids: " + directSubclassMap.keys.map( sc => sc.id ))
//        
//        val variableDeclarations = Loader.variableDeclarations
//        assertNotNull(variableDeclarations)
//        // map from identifier into a set of possible subclasses
//        var variableRefinements: MutableMap[Identifier, MutableSet[ClassType]] = MutableMap.empty
//        for (varDec <- variableDeclarations) {
//          varDec match {
//            case Declaration(ImmediateExpression(_, LeonVariable(id)), _, typeOfVar: ClassType) =>
//              variableRefinements += (id -> MutableSet(directSubclassMap(typeOfVar).toList: _*))
//            case _ =>
//          }
//        }
//				        
//        allDeclarations =
//          for (dec <- allDeclarations)
//            yield dec match {
//            case Declaration(imex @ ImmediateExpression(_, LeonVariable(id)), inSynthType, decClassType: ClassType)
//            	if id.name == "l1" || id.name == "l2" =>
//              println("decClassType: " + decClassType)
//              println("decClassType.id: " + decClassType.id)
//              assertTrue(directSubclassMap(decClassType).size > 0)    
//              val consClassType = directSubclassMap(decClassType) filter { clazz => clazz.classDef.id.name == "Cons"  }
//              println("consClassType: " + consClassType)
//              Declaration(
//                imex, TypeTransformer(consClassType.head), consClassType.head)
//            case _ =>
//              dec
//          }
//				
//        val newBuilder = new InitialEnvironmentBuilder
//        newBuilder.addDeclarations(allDeclarations)
//
//      	val proofTree = inSynth.getSnippets(newBuilder)
//				
//				assertNotNull( proofTree )
//				
//		    val codeGenerator = new CodeGenerator
//		    
//		    assertEquals(1, proofTree.getNodes.size)
//		    
//				//RecConfig.numberOfCombinations = numberOfSnippets
//        {
//        RecConfig.useEnumerationOrdering = true
//        
//				val snippets = Reconstructor(proofTree.getNodes.head, codeGenerator)
//				
//				val transformedStreamable = OrderedExtractor.transformedStreamable.asInstanceOf[
//				  ch.epfl.insynth.reconstruction.streams.oredered.Streamable[Node]
//			  ]			
//                            
//        val streamToTest = transformedStreamable.getStream.map(codeGenerator.apply(_)) zip transformedStreamable.getValues
//				println("evaluating results")
//				for ((snippetTree, ind) <- streamToTest zip Iterator.range(0,2000).toStream ) {
//		      println(ind + " snippetTree is: " + snippetTree)				    
//			  }
//                
////        println("transformedStreamable: " + 
////            ch.epfl.insynth.reconstruction.streams.oredered.FormatStreamUtils(transformedStreamable))
////            
////        import ch.epfl.insynth.reconstruction.streams.oredered._
////            
////        transformedStreamable match {
////			    case us: UnaryStream[_, _] =>
////			      us.streamable match {
////			        case bs: BinaryStream[_, _, _] =>
////			          bs.s2 match {
////			            case rr:RoundRobbin[Node] =>
////			              println("rr.size: " + rr.streams.size)
////			              for (rrr <- rr.streams) {
////              				println("asdasdasdasdsd results")
////											for ((snippetTree, ind) <- (rrr.getStream map { codeGenerator.transform(_).head }) zip Iterator.range(0,100).toStream ) {
////									      println(ind + " snippetTree is: " + snippetTree)				    
////										  }
////			              }
////			          }
////			      }
////        	}
//        
//        
//        }
////        println("###")
////        
////        {
////        RecConfig.useEnumerationOrdering = false
////        
////				val snippets = Reconstructor(proofTree.getNodes.head, codeGenerator)
////				
////				val transformedStreamable =Extractor.transformedStreamable
////				
////	        println("transformedStreamable: " + 
////	            ch.epfl.insynth.reconstruction.streams.FormatStreamUtils(transformedStreamable))
////	            
////	            
////        import ch.epfl.insynth.reconstruction.streams._
////            
////        transformedStreamable match {
////			    case us: UnaryStream[_, _] =>
////			      us.streamable match {
////			        case bs: BinaryStream[_, _, _] =>
////			          bs.s2 match {
////			            case rr:RoundRobbin[Node] =>
////			              println("rr.size: " + rr.streams.size)
////			              for (rrr <- rr.streams) {
////              				println("asdasdasdasdsd results")
////											for ((snippetTree, ind) <- (rrr.getStream map { codeGenerator.transform(_).head }) zip Iterator.range(0,100).toStream ) {
////									      println(ind + " snippetTree is: " + snippetTree)				    
////										  }
////			              }
////			          }
////			      }
////        	}
////        }
//								
//		  case _ =>		    
//		  	fail("Globals.program, Globals.hole")
//		}	
//	}	
//    
//	@Test
//	def testSnippetsListOperations4 = {
//	  runOnFile( "testcases/insynth/ListOperationsHole2.scala" )
//				
//		(Globals.program, Globals.hole) match {
//		  case (Some(program), hole: Hole) => 		
//				val inSynth = new InSynth(program, hole)
//												
//				inSynth.getSnippets
//								
//        // update declarations
//        var allDeclarations = inSynth.allDeclarations
//        
//        println("all declarations: " + allDeclarations.mkString("\n"))
//                
//        // each variable of super type can actually have a subtype
//        // get sine declaration maps to be able to refine them
//        val directSubclassMap = Loader.directSubclassesMap        
//        println("directSubclassMap: " + directSubclassMap)      
//        println("directSubclassMap ids: " + directSubclassMap.keys.map( sc => sc.id ))
//        
//        val variableDeclarations = Loader.variableDeclarations
//        assertNotNull(variableDeclarations)
//        // map from identifier into a set of possible subclasses
//        var variableRefinements: MutableMap[Identifier, MutableSet[ClassType]] = MutableMap.empty
//        for (varDec <- variableDeclarations) {
//          varDec match {
//            case Declaration(ImmediateExpression(_, LeonVariable(id)), _, typeOfVar: ClassType) =>
//              variableRefinements += (id -> MutableSet(directSubclassMap(typeOfVar).toList: _*))
//            case _ =>
//          }
//        }
//				        
//        allDeclarations =
//          for (dec <- allDeclarations)
//            yield dec match {
//            case Declaration(imex @ ImmediateExpression(_, LeonVariable(id)), inSynthType, decClassType: ClassType)
//            	if id.name == "l1" || id.name == "l2" =>
//              println("decClassType: " + decClassType)
//              println("decClassType.id: " + decClassType.id)
//              assertTrue(directSubclassMap(decClassType).size > 0)    
//              val consClassType = directSubclassMap(decClassType) filter { clazz => clazz.classDef.id.name == "Cons"  }
//              println("consClassType: " + consClassType)
//              Declaration(
//                imex, TypeTransformer(consClassType.head), consClassType.head)
//            case _ =>
//              dec
//          }
//				
//        val newBuilder = new InitialEnvironmentBuilder
//        newBuilder.addDeclarations(allDeclarations)
//
//      	val proofTree = inSynth.getSnippets(newBuilder)
//				
//				assertNotNull( proofTree )
//				
//		    val codeGenerator = new CodeGenerator
//		    
//		    assertEquals(1, proofTree.getNodes.size)
//		    
//				//RecConfig.numberOfCombinations = numberOfSnippets
//        
//        RecConfig.useEnumerationOrdering = true
//        
//				val snippets = Reconstructor(proofTree.getNodes.head, codeGenerator)
//				
//				val transformedStreamable = OrderedExtractor.transformedStreamable.asInstanceOf[
//				  ch.epfl.insynth.reconstruction.streams.oredered.Streamable[Node]
//			  ]
//                	          							
//      	assertTrue((false /: (snippets map { _.getSnippet } zip Iterator.from(0).toStream take 2000) ) {
//      	  case (res, (snippetTree, ind)) =>
//        	  println("ind: " + ind + " snippetTree is: " + snippetTree)
//            snippetTree.toString match {
//			        case "Cons(l1.head, concat(l1.tail, l2))" |
//			        	"Cons(l1.head, concat(l2, l1.tail))" |
//			        	"Cons(l2.head, concat(l2.tail, l1))" |
//			        	"Cons(l2.head, concat(l1, l2.tail))" => 
//			        	  println("Found at: " + ind + " boolean snippetTree is: " + snippetTree)
//			        	  true
//			        case _ => res
//	          }
//      	})
//		  case _ =>		    
//		  	fail("Globals.program, Globals.hole")
//		}	
//	}	
//  
//  // inspect the expression if some refinements can be done
//  def checkRefinements(expr: Expr) = expr match {
//    case CaseClassInstanceOf(classDef, LeonVariable(id)) =>
//      Some((id, classDef))
//    case _ =>
//      None
//  }
//  
//  @Ignore
//	@Test
//	def testSnippetsListOperations2: Unit = {
//	  runOnFile( "testcases/insynth/ListOperationsHole2.scala" )
//				
//		(Globals.program, Globals.hole) match {
//		  case (Some(program), hole: Hole) => 		
//				val inSynth = new InSynth(program, hole)
//												
//				inSynth.getSnippets
//								
//        // update declarations
//        var allDeclarations = inSynth.allDeclarations
//        
//        println("all declarations: " + allDeclarations.mkString("\n"))
//                
//        // each variable of super type can actually have a subtype
//        // get sine declaration maps to be able to refine them
//        val directSubclassMap = Loader.directSubclassesMap        
//        println("directSubclassMap: " + directSubclassMap)      
//        println("directSubclassMap ids: " + directSubclassMap.keys.map( sc => sc.id ))
//        
//        val variableDeclarations = Loader.variableDeclarations
//        assertNotNull(variableDeclarations)
//        // map from identifier into a set of possible subclasses
//        var variableRefinements: MutableMap[Identifier, MutableSet[ClassType]] = MutableMap.empty
//        for (varDec <- variableDeclarations) {
//          varDec match {
//            case Declaration(ImmediateExpression(_, LeonVariable(id)), _, typeOfVar: ClassType) =>
//              variableRefinements += (id -> MutableSet(directSubclassMap(typeOfVar).toList: _*))
//            case _ =>
//          }
//        }
//				        
//        allDeclarations =
//          for (dec <- allDeclarations)
//            yield dec match {
//            case Declaration(imex @ ImmediateExpression(_, LeonVariable(id)), inSynthType, decClassType: ClassType)
//            	if id.name == "l1" || id.name == "l2" =>
//              println("decClassType: " + decClassType)
//              println("decClassType.id: " + decClassType.id)
//              assertTrue(directSubclassMap(decClassType).size > 0)    
//              val consClassType = directSubclassMap(decClassType) filter { clazz => clazz.classDef.id.name == "Cons"  }
//              println("consClassType: " + consClassType)
//              Declaration(
//                imex, TypeTransformer(consClassType.head), consClassType.head)
//            case _ =>
//              dec
//          }
//				
//        val newBuilder = new InitialEnvironmentBuilder
//        newBuilder.addDeclarations(allDeclarations)
//
//      	val proofTree = inSynth.getSnippets(newBuilder)
//				
//				assertNotNull( proofTree )
//				
//		    val codeGenerator = new CodeGenerator
//		    
//		    assertEquals(1, proofTree.getNodes.size)
//		    
//				//RecConfig.numberOfCombinations = numberOfSnippets
//				  
//				Reconstructor(proofTree.getNodes.head, codeGenerator)
//				
//			  val transformed = Extractor.transformedStreamable
//			  
//			  val codeGen = new CodeGenerator
//			  
//			  transformed match {
//			    case us: UnaryStream[_, _] =>
//			      val solutionsStream = us.streamable
//			      assertTrue(
//		          solutionsStream match {
//		            case rr:RoundRobbin[_] =>
//		              val paramStreams = rr.streams
//						      (false /: paramStreams) {
//						        case (_, us:UnaryStream[_, _])//(BinaryStream(idStream, _), _))
//						          if (us.streamable.isInstanceOf[BinaryStream[_, _, _]] &&		
//						              (us.streamable.asInstanceOf[BinaryStream[_, _, _]].s1.getStream.head match {
//							            case n@interm.Identifier(
//						                _, decl
//					                ) if decl.getType == Arrow( TSet(Const("Cons")), Const("List") ) &&
//					                	decl.expression.getSimpleName.startsWith("Inheritane") =>
//							            	true
//							            case _ =>
//							              false
//							          })) =>
//					            val transformedStream = us.getStream map {
//					              case t: Node =>
//					                val codeGenRes = codeGen.transform(t)
//					                assertTrue(codeGenRes.size == 1)
//					                codeGenRes.head
//				              }
////				            	for ((snippetTree, ind) <- transformedStream zip Iterator.from(0).toStream ) {
////							          println(ind + ": " + snippetTree)
////									        	  
////								        if (ind % 100 == 0) {
////									        System.out.println("Press Any Key To Continue...");
////									        new java.util.Scanner(System.in).nextLine();
////								        }
////				            	}
//				            								          							
//				            	assertTrue((false /: (transformedStream zip Iterator.from(0).toStream take 10000) ) {
//				            	  case (res, (snippetTree, ind)) =>
//							        	  println("ind: " + ind + " snippetTree is: " + snippetTree)
//					                snippetTree.toString match {
//										        case "Cons(l1.head, concat(l1.tail, l2))" |
//										        	"Cons(l1.head, concat(l2, l1.tail))" |
//										        	"Cons(l2.head, concat(l2.tail, l1))" |
//										        	"Cons(l2.head, concat(l1, l2.tail))" => 
//										        	  println("Found at: " + ind + " boolean snippetTree is: " + snippetTree)
//										        	  true
//										        case _ => res
//								          }
//				            	})
//				            	
////		            	    assertTrue(
////	            	        transformedStream zip Iterator.from(0).toStream
////            	        )
//				            	true
//						        case (res, _) => res
//						      }
//		            case _ => false
//		          }
//			      )
//			    case _ => 
//			      fail
//			  }
//		  case _ =>		    
//		  	fail("Globals.program, Globals.hole")
//		}	
//	  
//	  //val snippets = getSnippets("testcases/insynth/ListOperationsHole2.scala")
//			  
////		println("evaluating results")
////		for ((snippetTree, ind) <- (snippets map { _.getSnippet }) zip Iterator.from(0).toStream ) {
////      		  
////		  
//////      snippetTree.toString match {
//////        case "Cons(l1.head, concat(l1.tail, l2))" |
//////        	"Cons(l1.head, concat(l2, l1.tail))" |
//////        	"Cons(l2.head, concat(l2.tail, l1))" |
//////        	"Cons(l2.head, concat(l1, l2.tail))" => 
//////        	  println("Found at: " + ind + " boolean snippetTree is: " + snippetTree)
//////        	  return
//////        case _ =>
//////          
//////        if (ind % 20 == 0) {
//////	        System.out.println("Press Any Key To Continue...");
//////	        new java.util.Scanner(System.in).nextLine();
//////        }
//////      }
////          
////	  }
//	}
//  
//  @Ignore
//  @Test
//	def testSnippetsListOperations5: Unit = {
//	  runOnFile( "testcases/insynth/ListOperationsHole2.scala" )
//				
//		(Globals.program, Globals.hole) match {
//		  case (Some(program), hole: Hole) => 		
//				val inSynth = new InSynth(program, hole)
//												
//				inSynth.getSnippets
//								
//        // update declarations
//        var allDeclarations = inSynth.allDeclarations
//        
//        println("all declarations: " + allDeclarations.mkString("\n"))
//                
//        // each variable of super type can actually have a subtype
//        // get sine declaration maps to be able to refine them
//        val directSubclassMap = Loader.directSubclassesMap        
//        println("directSubclassMap: " + directSubclassMap)      
//        println("directSubclassMap ids: " + directSubclassMap.keys.map( sc => sc.id ))
//        
//        val variableDeclarations = Loader.variableDeclarations
//        assertNotNull(variableDeclarations)
//        // map from identifier into a set of possible subclasses
//        var variableRefinements: MutableMap[Identifier, MutableSet[ClassType]] = MutableMap.empty
//        for (varDec <- variableDeclarations) {
//          varDec match {
//            case Declaration(ImmediateExpression(_, LeonVariable(id)), _, typeOfVar: ClassType) =>
//              variableRefinements += (id -> MutableSet(directSubclassMap(typeOfVar).toList: _*))
//            case _ =>
//          }
//        }
//				        
//        allDeclarations =
//          for (dec <- allDeclarations)
//            yield dec match {
//            case Declaration(imex @ ImmediateExpression(_, LeonVariable(id)), inSynthType, decClassType: ClassType)
//            	if id.name == "l1" || id.name == "l2" =>
//              println("decClassType: " + decClassType)
//              println("decClassType.id: " + decClassType.id)
//              assertTrue(directSubclassMap(decClassType).size > 0)    
//              val consClassType = directSubclassMap(decClassType) filter { clazz => clazz.classDef.id.name == "Cons"  }
//              println("consClassType: " + consClassType)
//              Declaration(
//                imex, TypeTransformer(consClassType.head), consClassType.head)
//            case _ =>
//              dec
//          }
//				
//        val newBuilder = new InitialEnvironmentBuilder
//        newBuilder.addDeclarations(allDeclarations)
//
//      	val proofTree = inSynth.getSnippets(newBuilder)
//				
//				assertNotNull( proofTree )
//				
//		    val codeGenerator = new CodeGenerator
//		    
//		    assertEquals(1, proofTree.getNodes.size)
//		    
//				//RecConfig.numberOfCombinations = numberOfSnippets
//				  
//				Reconstructor(proofTree.getNodes.head, codeGenerator)
//				
//			  val transformed = Extractor.transformedStreamable
//			  
//			  val codeGen = new CodeGenerator
//			  
//			  transformed match {
//			    case us: UnaryStream[_, _] =>
//			      val solutionsStream = us.streamable
//
//		        println("solutionsStream: " + FormatStreamUtils(solutionsStream))
//	          solutionsStream match {
//	            case rr:RoundRobbin[_] =>
//	              val paramStreams = rr.streams
//					      (false /: paramStreams) {
//					        case (_, us:UnaryStream[_, _])//(BinaryStream(idStream, _), _))
//					          if (us.streamable.isInstanceOf[BinaryStream[_, _, _]] &&		
//					              (us.streamable.asInstanceOf[BinaryStream[_, _, _]].s1.getStream.head match {
//						            case n@interm.Identifier(
//					                _, decl
//				                ) if decl.getType == Arrow( TSet(Const("Cons")), Const("List") ) &&
//				                	decl.expression.getSimpleName.startsWith("Inheritane") =>
//						            	true
//						            case _ =>
//						              println("if false")
//					            		false
//						          })) =>
//				            val transformedStream = us.getStream map {
//				              case t: Node =>
//				                val codeGenRes = codeGen.transform(t)
//				                assertTrue(codeGenRes.size == 1)
//				                codeGenRes.head
//			              }
////				            	for ((snippetTree, ind) <- transformedStream zip Iterator.from(0).toStream ) {
////							          println(ind + ": " + snippetTree)
////									        	  
////								        if (ind % 100 == 0) {
////									        System.out.println("Press Any Key To Continue...");
////									        new java.util.Scanner(System.in).nextLine();
////								        }
////				            	}
//			            								          							
//			            	assertTrue((false /: (transformedStream zip Iterator.from(0).toStream take 10000) ) {
//			            	  case (res, (snippetTree, ind)) =>
//				                snippetTree.toString match {
//									        case "Cons(l1.head, concat(l1.tail, l2))" |
//									        	"Cons(l1.head, concat(l2, l1.tail))" |
//									        	"Cons(l2.head, concat(l2.tail, l1))" |
//									        	"Cons(l2.head, concat(l1, l2.tail))" => 
//									        	  println("Found at: " + ind + " boolean snippetTree is: " + snippetTree)
//									        	  true
//									        case _ => res
//							          }
//			            	})
//			            	
////		            	    assertTrue(
////	            	        transformedStream zip Iterator.from(0).toStream
////            	        )
//			            	true
//					        case (res, _) => res
//					      }
//	            case _ =>
//	            	fail
//	          }
//			      
//			    case _ => 
//			      fail
//			  }
//		  case _ =>		    
//		  	fail("Globals.program, Globals.hole")
//		}
//  }
	
}
