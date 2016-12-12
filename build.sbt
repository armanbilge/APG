name := "APG"
version := "0.1"
organization := "org.compevol"
scalaVersion := "2.12.1"
val monocleVersion = "1.3.2"
libraryDependencies += "com.github.julien-truffaut" %%  "monocle-core" % monocleVersion
libraryDependencies += "com.github.julien-truffaut" %%  "monocle-macro" % monocleVersion
libraryDependencies += "com.github.julien-truffaut" %%  "monocle-state" % monocleVersion
libraryDependencies += "com.github.julien-truffaut" %%  "monocle-refined" % monocleVersion
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)
