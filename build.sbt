name := "APG"
version := "0.1"
organization := "org.compevol"
scalaVersion := "2.12.4"
crossScalaVersions := Seq("2.11.11", scalaVersion.value)
scalacOptions := { scalaVersion.value match {
  case "2.11.11" => Seq("-optimize")
  case _ => Seq("-opt:l:classpath")
}}
resolvers += Resolver.sonatypeRepo("snapshots")
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"
libraryDependencies += "net.jcazevedo" %% "moultingyaml" % "0.4.1-SNAPSHOT"
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
libraryDependencies ++= { scalaVersion.value match {
  case "2.11.11" => Seq("org.apache.spark" %% "spark-core" % "2.2.0" % Provided)
  case _ => Seq.empty
}}
libraryDependencies += "org.compevol" %% "mcmc" % "0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

lazy val compileNative = taskKey[Seq[File]]("Compile native code")
compileNative := {
  resourceManaged.value.mkdirs()
  val out = resourceManaged.value / "apg.jni"
  Seq("sh", "-c", s"cc -O3 -ffast-math -fPIC -march=native -mtune=native $$CFLAGS -lc -shared -o $out src/main/c/apg/*.c").!
  Seq(out)
}
(compile in Compile) <<= (compile in Compile).dependsOn(compileNative)
resourceGenerators in Compile += compileNative.taskValue
