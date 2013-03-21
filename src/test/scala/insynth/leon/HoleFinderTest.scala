package insynth.leon

import org.junit.Assert._
import org.junit.{ Test, Ignore }

import insynth.structures._
import insynth.leon.loader.LeonLoader
import insynth.leon.{ LeonDeclaration => Declaration }

import leon.purescala.Trees.Hole
import leon.purescala.TypeTrees.Int32Type
import leon.purescala.Definitions.Program

import testutil.TestConfig

class HoleFinderTest {

  import TestConfig._

  @Test
  def testDesiredTypeExtraction {

    val holeFinder = new HoleFinder(testDir + "Hole.scala")

    checkResult(holeFinder.extract)

    //checkResult(holeFinder.extractWithRun(classpathArray))
  }

  private def checkResult(result: Option[(Program, Hole)]) = result match {
    case Some((prog, Hole(tpe))) =>
      assertEquals(Int32Type, tpe)
    case _ =>
      fail
  }

}
