name := "InSynth"

version := "2.1"

organization := "ch.epfl.lara"

scalaVersion := "2.11.8"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

javacOptions += "-Xlint:unchecked"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

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

// default setting excludes packaging main logging config file
packageMainLogConfig := false

// temporary dependency issue
//ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

EclipseKeys.useProjectId := true
