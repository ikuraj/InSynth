package insynth.leon

import org.junit.Assert._
import org.junit.{ Test, Ignore }

import insynth.structures._
import insynth.leon.loader.LeonLoader
import insynth.leon.{ LeonDeclaration => Declaration }

import leon.purescala.TypeTrees._
import leon.purescala.Trees.{ Variable => LeonVariable, _ }
import leon.purescala.Definitions.Program

import testutil.TestConfig

class LeonLoaderTest {

  import TestConfig._

  @Test
  def testClassMap {
    val holeFinder = new HoleFinder(testDir + "CaseClassSelectExample.scala")

    holeFinder.extract match {
      case Some((prog, hole)) =>
        val leonLoader = new LeonLoader(prog, hole)

        val extractedClasses = leonLoader.extractClasses

        assertEquals(prog.definedClasses.size, extractedClasses.size)

        for (classDef <- prog.definedClasses; val classId = classDef.id)
          assertNotNull(extractedClasses(classId))
      case None =>
        fail
    }

  }

  @Test
  def testHierarchyExtraction {

    val holeFinder = new HoleFinder(testDir + "RedBlackTreeFull.scala")

    holeFinder.extract match {
      case Some((prog, hole)) =>
        val leonLoader = new LeonLoader(prog, hole)

        assertNotNull(prog)

        val inheritanceDeclarations = leonLoader.extractInheritances

        for (
          inheritancePair <- List(
            Pair("Red", "Color"), Pair("Black", "Color"), Pair("Empty", "Tree"),
            Pair("Some", "OptionInt"), Pair("None", "OptionInt"))
        ) {
          assertTrue(
            "Found declarations:\n" + inheritanceDeclarations.mkString("\n"),
            (false /: inheritanceDeclarations) {
              (res, decl) =>
                {
                  decl match {
                    case Declaration(Arrow(TSet(List(Const(inheritancePair._1))), Const(inheritancePair._2)), _, _, _) => true
                    case _ => res
                  }
                }
            })
        }
      case None =>
        fail
    }
  }

  @Test
  def testArgumentDeclarationsExtraction {

    val holeFinder = new HoleFinder(testDir + "Arguments.scala")

    holeFinder.extract match {
      case Some((prog, hole)) =>
        assertEquals(1, prog.definedFunctions.size)

        val leonLoader = new LeonLoader(prog, hole)

        val declarations = leonLoader.load

        for (
          (name, tpe) <- List(("t", Const("Int")), ("x", Const("Boolean")), ("y", Const("Boolean")))
        ) {
          assertTrue(
            "Error: " + name + " should be found. Local declarations found: " + declarations.mkString("\n"),
            (false /: declarations) {
              case (true, _) => true
              case (_, Declaration(`tpe`, _, _, ImmediateExpression(_, expr))) =>
                expr match {
                  case LeonVariable(id) if id.name == name => true
                  case _ => false
                }
              case _ => false
            })
        }
      case _ =>
        fail
    }

  }

  @Test
  def testCaseClassExtraction {

    val holeFinder = new HoleFinder(testDir + "CaseClassSelectExample.scala")

    holeFinder.extract match {
      case Some((prog, hole)) =>

        assertEquals(3, prog.definedClasses.size)

        val leonLoader = new LeonLoader(prog, hole)

        val extractedFields = leonLoader.extractFields

        assertEquals(1, extractedFields.size)

        assertTrue(
          "Case class declarations found: " + extractedFields.mkString(", "),
          (false /: extractedFields) {
            (res, extrField) =>
              extrField match {
                case Declaration(Arrow(TSet(List(Const("Some"))), Const("Int")), _, _, _) =>
                  true
                case _ => res
              }
          })

        val extractedCaseClasses = leonLoader.extractCaseClasses

        val caseClassesDeclarationsString =
          "Field declarations found: " + extractedCaseClasses.mkString(", ")

        assertEquals(2, extractedCaseClasses.size)

        for (
          (name, tpe) <- List(
            ("Cst(Some)", Arrow(TSet(Const("Int")), Const("Some"))),
            ("Cst(None)", Const("None")))
        ) assertTrue(
          caseClassesDeclarationsString,
          (false /: extractedCaseClasses) {
            (res, extrField) =>
              extrField match {
                case Declaration(`tpe`, _, _, expr) if expr.getSimpleName == name =>
                  true
                case _ => res
              }
          })

        val extractedCaseClassDependent = leonLoader.extractClassDependentDeclarations

        val extractedCaseClassDependentString =
          "instanceof declarations found: " + extractedCaseClassDependent.mkString(", ")

        println(extractedCaseClassDependentString)
        assertEquals(2, extractedCaseClassDependent.size)

        for (
          (name, tpe) <- List(
            ("Some", Arrow(TSet(Const("OptionInt")), Const("Boolean"))),
            ("None", Arrow(TSet(Const("OptionInt")), Const("Boolean"))))
        ) assertTrue(
          extractedCaseClassDependentString,
          (false /: extractedCaseClassDependent) {
            (res, extrField) =>
              extrField match {
                case Declaration(`tpe`, _, _, expr: UnaryReconstructionExpression) if expr(UnitLiteral) == CaseClassInstanceOf(prog.caseClassDef(name), UnitLiteral) =>
                  true
                case _ => res
              }
          })

      case _ =>
        fail
    }
  }

  @Test
  def testCaseClassExtraction2 {

    val holeFinder = new HoleFinder(testDir + "ListOperationsHole.scala")

    holeFinder.extract match {
      case Some((prog, hole)) =>

        assertEquals(3, prog.definedClasses.size)

        val leonLoader = new LeonLoader(prog, hole)

        val extractedDeclarations = leonLoader.load

        val extractedDeclarationsString = extractedDeclarations mkString "\n"

        // field		
        for (
          tpe <- List(
            Arrow(TSet(Const("Cons")), Const("Int")),
            Arrow(TSet(Const("Cons")), Const("List")))
        ) assertTrue(
          "Case class field declarations not found. Found: " + extractedDeclarationsString,
          (false /: extractedDeclarations) {
            (res, extr) =>
              extr match {
                case Declaration(`tpe`, _, _, _) =>
                  true
                case _ => res
              }
          })

        for (
          (name, tpe) <- List(
            ("Cst(Cons)", Arrow(TSet(Const("Int"), Const("List")), Const("Cons"))),
            ("Cst(Nil)", Const("Nil")))
        ) assertTrue(
          "Case class constructor tpe(" + tpe + ") not found. Found: " + extractedDeclarationsString,
          (false /: extractedDeclarations) {
            (res, extr) =>
              extr match {
                case Declaration(`tpe`, _, _, expr) if expr.getSimpleName == name =>
                  true
                case _ => res
              }
          })

        for (
          (name, tpe) <- List(
            ("Cons", Arrow(TSet(Const("List")), Const("Boolean"))),
            ("Nil", Arrow(TSet(Const("List")), Const("Boolean"))))
        ) assertTrue(
          "instanceOf not found. Found: " + extractedDeclarationsString,
          (false /: extractedDeclarations) {
            (res, extr) =>
              extr match {
                case Declaration(`tpe`, _, _, expr: UnaryReconstructionExpression) if expr(UnitLiteral) == CaseClassInstanceOf(prog.caseClassDef(name), UnitLiteral) =>
                  true
                case _ => res
              }
          })

      case _ =>
        fail
    }
  }

  @Test
  def testDirectSubclassExtraction {

    val holeFinder = new HoleFinder(testDir + "ListOperationsHole.scala")

    holeFinder.extract match {
      case Some((prog, hole)) =>
        assertEquals(3, prog.definedClasses.size)

        val leonLoader = new LeonLoader(prog, hole)

        // TODO this should not be required to get direct subclass map
        val extractedDeclarations = leonLoader.load

        val subclassesMap = leonLoader.directSubclassesMap
        assertNotNull(subclassesMap)

        for (
          pair @ (classTypeName, nameSet) <- List(
            ("Cons", Set()),
            ("Nil", Set()),
            ("List", Set("Nil", "Cons")))
        ) assertTrue(
          "pair " + pair + " not found. Found: " + subclassesMap.mkString(", "),
          (false /: subclassesMap) {
            (res, extr) =>
              extr match {
                case (classType, classTypeSet) => res ||
                  (
                    classType.classDef.id.name == classTypeName &&
                    (true /: nameSet) {
                      (res, innerName) => res && classTypeSet.exists(_.classDef.id.name == innerName)
                    })
                case _ => false
              }
          })

      case _ =>
        fail
    }
  }

  // TODO this fails after pulling newest leon
  //  	@Ignore
  //  	@Test
  //  	def testLocalDeclarationsExtractionWithHides {	  
  //  		runOnFile( "testcases/insynth/LocalScope.scala" )
  //  		
  //  		val prog: Program = 
  //  			Globals.program match {
  //  			  case None => 
  //  			    fail
  //  			    return
  //  			  case Some(prog) =>		    
  //  			  	prog
  //  			}
  //  		
  //  		assertEquals(1, prog.definedFunctions.size)
  //  		
  //  		val body = prog.definedFunctions.head.getBody
  //  		
  //  		println("got body: " + body)
  //  		
  //  		val localDeclarations = Loader.loadLocals(body, Globals.hole)
  //  				
  //  		//		println( 
  //  //		    "Trees: " + ((localDeclarations filter {
  //  //				  case Declaration(expr: (List[Expr] => Expr), Const("Tree"), _) => true
  //  //				}) map { _.expression.asInstanceOf[List[Expr] => Expr](Nil).asInstanceOf[LeonVariable].id } )
  //  //		)
  //  		
  //  		for ( 
  //  	    (name, tpe) <- 
  //  	    List(
  //  	      ("l", Const("Tree")), ("v", Const("Int")), ("r", Const("Int")), 
  //          ("newInt", Const("Int"))
  //  //        , 
  //  //        ("t", Const("Tree"))
  //        )
  //      ) {
  //  		  assertTrue(
  //  	      "Error: " + name + " should be found. Local declarations found: " + localDeclarations.mkString(", "),
  //  	      (false /: localDeclarations) {
  //  	        case (true, _) => true
  //  	        case ( _, Declaration(ImmediateExpression(_, expr), `tpe`, _) ) =>
  //  	          expr match {
  //  	            case LeonVariable(id) if id.name == name => true
  //  	            case _ => false
  //  	          }
  //  	        case _ => false
  //  	      }
  //        )
  //  		}		
  //  		
  //  		for ( 
  //  	    (name, tpe) <- 
  //  	    List( ( "f", Arrow(TSet(List(Const("Int"))), Const("Boolean")) )
  //  //	        ,  ("r", Const("Tree")) 
  //        )
  //      ) {
  //  		  assertFalse(
  //  	      "Error: " + name + " should not be found. Local declarations found: " + localDeclarations.mkString(", "),
  //  	      (false /: localDeclarations) {
  //  	        case (true, _) => true
  //  	        case ( _, Declaration(ImmediateExpression(_, expr), `tpe`, _) ) =>
  //  	          expr match {
  //  	            case LeonVariable(id) if id.name == name => true
  //  	            case _ => false
  //  	          }
  //  	        case _ => false
  //  	      }
  //        )
  //  		}
  //  		
  //  	}

}
