
name := "Scalisp"

version := "1.1.1""

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scala-lang.modules" % "scala-parser-combinators_2.11" % "1.0.4",
  ("org.scala-lang" % "jline" % "2.10.6")
      .exclude("org.fusesource.jansi", "jansi")
        .exclude("jline", "jline"),
  "org.clapper" %% "argot" % "1.0.4"
)


assemblyMergeStrategy in assembly := {
        case n if n.endsWith("linux32/libjansi.so")                  => MergeStrategy.discard
        case n if n.endsWith("linux64/libjansi.so")                  => MergeStrategy.discard
        case n if n.endsWith("osx/libjansi.jnilib")                  => MergeStrategy.discard
        case n if n.endsWith("jansi.dll")                            => MergeStrategy.discard
        case n if n.startsWith("org/fusesource/jansi/")              => MergeStrategy.last
        case n if n.startsWith("org/fusesource/hawtjni/runtime/")    => MergeStrategy.last
        case x =>
                val oldStrategy = (assemblyMergeStrategy in assembly).value
                oldStrategy(x)
        }
}
