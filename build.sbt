name := "InSynth"

version := "2.1"

organization := "ch.epfl.lara"

scalaVersion := "2.12.13"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

javacOptions += "-Xlint:unchecked"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

libraryDependencies += "junit" % "junit" % "4.8" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10-M3" % "test"

// logging facilities
libraryDependencies ++= Seq(
  //"com.typesafe" %% "scalalogging-log4j" % "1.0.1",
  "org.apache.logging.log4j" % "log4j-api" % "2.0-beta3",
  "org.apache.logging.log4j" % "log4j-core" % "2.0-beta3"
)

//seq(ScctPlugin.instrumentSettings : _*)

//coverageEnabled := true

// temporary dependency issue
//ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

val packageMainLogConfig = settingKey[Boolean]("The version of Scala used for building.")

// default setting excludes packaging main logging config file
packageMainLogConfig := false

(Compile / packageBin / mappings) ++= {
  if (packageMainLogConfig.value)
    Seq((baseDirectory.value / "res" / "log4j2.xml") -> "resources/log4j2.xml")
  else
    Nil
}

lazy val root = Project(
  id = "InSynth-engine",
  base = file(".")
)

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
