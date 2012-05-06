import AssemblyKeys._ // put this at the top of the file

seq(assemblySettings: _*)

name := "Scalisp"

version := "1.0"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.7.2" % "test"
)

libraryDependencies <+= scalaVersion apply ("org.scala-lang" % "jline" % _)

libraryDependencies += "org.clapper" %% "argot" % "0.4"