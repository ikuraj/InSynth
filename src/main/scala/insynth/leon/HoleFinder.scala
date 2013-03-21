package insynth.leon

import scala.tools.nsc.{ Settings => NSCSettings, MainGenericRunner }

import leon.{ Main => LeonMain, DefaultReporter }
import leon.purescala.TypeTrees._
import leon.purescala.Trees.{ Variable => LeonVariable, _ }
import leon.purescala.Definitions.{ FunDef, VarDecl, Program, ObjectDef }
import leon.purescala.Common.{ Identifier, FreshIdentifier }
import leon.plugin.ExtractionPhase

import lesynth.Globals

class HoleFinder(fileName: String) {
  
  val args = Array(fileName, "--timeout=2")	

  def extract = {
    // do not need the overhead of running the whole thing    
		import LeonMain.processOptions
        
    val reporter = new DefaultReporter	  
		val ctx = processOptions(reporter, args.toList)
    Globals.program = Some(ExtractionPhase.run(ctx)(List(fileName)))

    Globals.program match {
      case Some(program) =>
        Some((program, Globals.hole))
      case _ =>
        None
    }
  }
  
//  def extractWithRun(classpathArray: Array[String]) = {
//  	val SCALACLASSPATH = classpathArray mkString ":"
//  	
//    //LeonMain.run(Array(testDir + "RedBlackTree.scala", "--timeout=3", "--noLuckyTests"), new DefaultReporter, Some(List(SCALACLASSPATH)))
//    LeonMain.run(args, new DefaultReporter, Some(List(SCALACLASSPATH)))
//        
//    Globals.program match {
//      case Some(program) =>
//        Some((program, Globals.hole))
//      case _ =>
//        None
//    }
//  }
}