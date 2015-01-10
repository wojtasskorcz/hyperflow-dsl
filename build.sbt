lazy val root = (project in file(".")).

settings(
  name := "hyperflow-dsl",
  version := "1.0",
  scalaVersion := "2.11.2"
)

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
libraryDependencies += "junit" % "junit" % "4.12" % "test"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:reflectiveCalls")
