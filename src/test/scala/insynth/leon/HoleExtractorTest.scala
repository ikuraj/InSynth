package insynth.leon

import org.junit.Assert._
import org.junit.{ Test, Ignore }

import insynth.structures._
import insynth.leon.loader.{ LeonLoader, HoleExtractor, _ }
import insynth.leon.{ LeonDeclaration => Declaration }

import leon.purescala.TypeTrees._
import leon.purescala.Trees.{ Variable => LeonVariable, _ }
import leon.purescala.Definitions.Program

import testutil.TestConfig

class HoleExtractorTest {

  import TestConfig._

  @Test
  def testLocalDeclarationsExtraction {

    val holeFinder = new HoleFinder(testDir + "BubbleSortBug.scala")

    checkResult(holeFinder.extract)

    //checkResult(holeFinder.extractWithRun(classpathArray))

  }

  private def checkResult(result: Option[(Program, Hole)]) = result match {
    case Some((prog, hole)) =>

      assertEquals(1, prog.definedFunctions.size)

      val body = prog.definedFunctions.head.getBody

      val holeExtractor = new HoleExtractor(prog, hole)

      holeExtractor.extractHole match {
        case Some((_, localDeclarations)) =>

          val declrationsString =
            (
              for (Declaration(_, _, tpe, expr) <- localDeclarations)
                yield expr + "(" + tpe + ")").mkString(", ")

          for (
            (name, tpe) <- List(
              //("i", Const("Int")), ("j", Const("Int")), // not supported due to recent removal of LetVar from Leon
              ("tmp", Const("Int")), ("sa", Instance("Array", List(Const("Int")))))
          ) {
            assertTrue(
              "Not found " + name + ". Local declarations found: " + declrationsString,
              (false /: localDeclarations) {
                case (true, _) => true
                case (_, Declaration(`tpe`, _, _, ImmediateExpression(_, expr))) =>
                  expr match {
                    case LeonVariable(id) if id.name == name => true
                    case _ => false
                  }
                case _ => false
              })
          }

          for (
            (name, tpe) <- List(("ij", Const("Int")), ("j", Const("Boolean")))
          ) {
            assertFalse(
              "Error: " + name + " should not be found. Local declarations found: " + localDeclarations.mkString(", "),
              (false /: localDeclarations) {
                case (true, _) => true
                case (_, Declaration(_, _, `tpe`, ImmediateExpression(_, expr))) =>
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
    case _ =>
      fail
  }

}
