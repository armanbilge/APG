name := "APG"
version := "0.1"
organization := "org.compevol"
scalaVersion := "2.12.2"
crossScalaVersions := Seq("2.11.8", scalaVersion.value)
resolvers += Resolver.sonatypeRepo("snapshots")
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"
libraryDependencies += "net.jcazevedo" %% "moultingyaml" % "0.4.1-SNAPSHOT"
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
libraryDependencies ++= { scalaVersion.value match {
  case "2.11.8" => Seq("org.apache.spark" %% "spark-core" % "2.1.1" % Provided)
  case _ => Seq.empty
}}
libraryDependencies += "org.compevol" %% "mcmc" % "0.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
