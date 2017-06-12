name := "APG"
version := "0.1"
organization := "org.compevol"
scalaVersion := "2.11.8"
scalacOptions += "-optimize"
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"
libraryDependencies += "com.lihaoyi" % "ammonite" % "0.8.1" cross CrossVersion.full
libraryDependencies += "com.twitter" % "chill_2.11" % "0.9.2"
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
libraryDependencies += "org.apache.spark" %% "spark-core" % "2.1.0" % Provided
libraryDependencies += "org.compevol" %% "mcmc" % "0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)
