name := "APG"
version := "0.1"
organization := "org.compevol"
scalaVersion := "2.12.2"
crossScalaVersions := Seq("2.11.8", scalaVersion.value)
unmanagedSourceDirectories in Compile <+= (sourceDirectory in Compile, scalaBinaryVersion) {
  (s, v) => s / ("scala_"+v)
}
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"
libraryDependencies += "com.lihaoyi" % "ammonite" % "1.0.0-RC7" cross CrossVersion.full
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
libraryDependencies <++= scalaVersion {
  case "2.11.8" => Seq("org.apache.spark" %% "spark-core" % "2.1.1") // % Provided
  case _ => Seq.empty
}
libraryDependencies += "org.compevol" %% "mcmc" % "0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)
