import AssemblyKeys._ // put this at the top of the file

seq(assemblySettings: _*)

name := "Scalisp"

version := "1.0"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.6.1" % "test"
)