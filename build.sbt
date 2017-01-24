name := "APG"
version := "0.1"
organization := "org.compevol"
scalaVersion := "2.12.1"
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"
libraryDependencies += "com.lihaoyi" % "ammonite" % "0.8.1" cross CrossVersion.full
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
libraryDependencies += "org.compevol" %% "mcmc" % "0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)
