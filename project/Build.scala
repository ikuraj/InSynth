import sbt._
import Process._
import Keys._

object InSynth extends Build {

  val packageMainLogConfig = SettingKey[Boolean]("The version of Scala used for building.")

  val mappingSetting =
	  mappings in (Compile, packageBin) <++=
	    (baseDirectory, packageMainLogConfig) map { (base, setting) =>
		    if (setting)
		      Seq((base / "res" / "log4j2.xml") -> "resources/log4j2.xml")
		    else
		      Nil
		  }

  lazy val root = Project(
      id = "InSynth-engine",
      base = file("."),
      settings = Project.defaultSettings ++ Seq(mappingSetting)
    ).
      dependsOn(loggingHelpers % "compile->compile;test->test")

  // might require something like: git config --global url."git@github.mit.edu".insteadOf git://github.mit.edu
  lazy val loggingHelpers = ProjectRef(
    uri("git://github.mit.edu:ivanko/logging-helpers.git"),
    "logging-helpers")

}
