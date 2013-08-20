name := "InSynth"

version := "2.0"

organization := "ch.epfl.lara"

scalaVersion := "2.9.3"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

javacOptions += "-Xlint:unchecked"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies += "junit" % "junit" % "4.8" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10-M3" % "test"

//libraryDependencies += "com.dongxiguo" %% "zero-log" % "0.1.2"